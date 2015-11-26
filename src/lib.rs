extern crate tokenizer;

use std::{fmt, cmp};
use std::io::Read;
use std::sync::Arc;
use std::ops::Deref;
use std::iter::Iterator;
use std::hash::{Hash, Hasher};
use std::collections::VecDeque;
use tokenizer::Token;

#[derive(Clone, Eq)]
pub struct Ngram<T> {
    mem: Arc<Vec<T>>,
    len: usize,
}

impl<T> Ngram<T> {
    pub fn new(tokens: Vec<T>) -> Ngram<T> {
        let len = tokens.len();
        Ngram { mem: Arc::new(tokens), len: len, }
    }

    fn derive(&self) -> Ngram<T> {
        Ngram { mem: self.mem.clone(), len: self.len - 1, }
    }
}

impl<T> Hash for Ngram<T> where T: Hash {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.deref().hash(state);
    }
}

impl<T> cmp::PartialEq<Ngram<T>> for Ngram<T> where T: cmp::PartialEq<T> {
    fn eq(&self, other: &Ngram<T>) -> bool {
        self.deref().eq(other.deref())
    }
}

impl<T> fmt::Debug for Ngram<T> where T: fmt::Debug {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Ngram({:?})", self.deref())
    }
}

impl<T> Deref for Ngram<T> {
    type Target = [T];

    fn deref(&self) -> &[T] {
        let len = self.len;
        &self.mem[.. len]
    }
}

pub trait Filter<T> {
    fn accept(&mut self, ngram: &Ngram<Arc<T>>) -> bool;
}

#[derive(Debug)]
pub struct AcceptEverything;
impl<T> Filter<T> for AcceptEverything {
    fn accept(&mut self, _ngram: &Ngram<Arc<T>>) -> bool { true }
}

pub struct Ngrams<T, I, E, F> where I: Iterator<Item = Result<T, E>> {
    state: IterState<T>,
    filter: F,
    src: I,
    max_ngram: usize,
    window: VecDeque<Arc<T>>,
}

#[derive(Clone, Copy)]
enum IterCont {
    Continue,
    Finished,
}

#[derive(Clone)]
enum IterState<T> {
    Fill,
    Permute { len: usize, cont: IterCont, gen_type: NgramGenType<Arc<T>>, },
    Depleted,
}

impl<T, I, E, F> Ngrams<T, I, E, F> where I: Iterator<Item = Result<T, E>> {
    pub fn new(src: I, max_ngram: usize, filter: F) -> Ngrams<T, I, E, F> where F: Filter<T>
    {
        Ngrams {
            state: IterState::Fill,
            src: src,
            filter: filter,
            max_ngram: max_ngram,
            window: VecDeque::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum NgramGenType<T> {
    Base,
    Derived(Ngram<T>),
}

#[derive(Debug)]
pub struct NgramGen<T> {
    pub ngram: Ngram<Arc<T>>,
    pub gen_type: NgramGenType<Arc<T>>,
}

pub trait Attrs {
    fn require_flush(&self) -> bool;
    fn should_be_skipped(&self) -> bool;
}

impl Attrs for Token {
    fn require_flush(&self) -> bool {
        if let &Token::Newline = self { true } else { false }
    }

    fn should_be_skipped(&self) -> bool {
        if let &Token::Whitespaces(..) = self { true } else { false }
    }
}

impl<T, I, E, F> Iterator for Ngrams<T, I, E, F> where I: Iterator<Item = Result<T, E>>, F: Filter<T>, T: Attrs {
    type Item = Result<NgramGen<T>, E>;

    fn next(&mut self) -> Option<Result<NgramGen<T>, E>> {
        loop {
            enum Trans<T> {
                Keep,
                NextState(IterState<T>),
                ValueReady(NgramGen<T>, IterState<T>),
            }

            let trans = match &self.state {
                &IterState::Depleted =>
                    return None,
                &IterState::Fill if self.window.len() >= self.max_ngram =>
                    Trans::NextState(IterState::Permute { len: self.window.len(), cont: IterCont::Continue, gen_type: NgramGenType::Base, }),
                &IterState::Fill => match self.src.next() {
                    Some(Ok(ref token)) if token.require_flush() =>
                        Trans::NextState(IterState::Permute { len: self.window.len(), cont: IterCont::Continue, gen_type: NgramGenType::Base, }),
                    Some(Ok(ref token)) if token.should_be_skipped() =>
                        Trans::Keep,
                    Some(Ok(token)) => {
                        self.window.push_back(Arc::new(token));
                        Trans::Keep
                    },
                    Some(Err(e)) =>
                        return Some(Err(e)),
                    None if self.window.is_empty() =>
                        Trans::NextState(IterState::Depleted),
                    None =>
                        Trans::NextState(IterState::Permute { len: self.window.len(), cont: IterCont::Finished, gen_type: NgramGenType::Base, }),
                },
                &IterState::Permute { len: 0, cont: IterCont::Finished, .. } if self.window.is_empty() =>
                    Trans::NextState(IterState::Depleted),
                &IterState::Permute { len: 0, cont: IterCont::Finished, .. } => {
                    self.window.pop_front();
                    Trans::NextState(IterState::Permute { len: self.window.len(), cont: IterCont::Finished, gen_type: NgramGenType::Base, })
                },
                &IterState::Permute { len: 0, cont: IterCont::Continue, .. } => {
                    self.window.pop_front();
                    Trans::NextState(IterState::Fill)
                },
                &IterState::Permute { len: ngram_len, cont: cont_action, gen_type: ref gen_type_value, } => {
                    let ngram = match gen_type_value {
                        &NgramGenType::Base => Ngram::new(self.window.iter().take(ngram_len).cloned().collect()),
                        &NgramGenType::Derived(ref base) => base.derive(),
                    };
                    let next_state =
                        IterState::Permute { len: ngram_len - 1, cont: cont_action, gen_type: NgramGenType::Derived(ngram.clone()), };
                    if self.filter.accept(&ngram) {
                        Trans::ValueReady(NgramGen { ngram: ngram, gen_type: gen_type_value.clone(), }, next_state)
                    } else {
                        Trans::NextState(next_state)
                    }
                },
            };

            match trans {
                Trans::Keep =>
                    (),
                Trans::NextState(next_state) =>
                    self.state = next_state,
                Trans::ValueReady(ngram_gen, next_state) => {
                    self.state = next_state;
                    return Some(Ok(ngram_gen));
                },
            }
        }
    }
}

#[cfg(test)]
mod test {
    use std::sync::Arc;
    use std::ops::Deref;
    use std::cmp::Ordering;
    use tokenizer::{Token, Tokens};
    use super::{Ngram, Ngrams, AcceptEverything};

    #[test]
    fn simple_ngramms() {
        let string = "And by the way, is there a reason I can't simply git submodule rm whatever?";
        let tokens = Tokens::<_, ()>::new(string.chars().flat_map(|c| c.to_lowercase().map(|lc| Ok(lc))));
        let ngrams_iter = Ngrams::new(tokens, 3, AcceptEverything);
        let mut ngrams: Vec<_> = ngrams_iter.map(|g| g.unwrap().ngram).collect();
        ngrams.sort_by(|a, b| match a.len().cmp(&b.len()) {
            Ordering::Equal => a.deref().cmp(b),
            other => other,
        });

        macro_rules! w { ($str:expr) => ({ Arc::new(Token::PlainWord($str.to_owned())) }) }
        macro_rules! p { ($str:expr) => ({ Arc::new(Token::Punct($str.to_owned())) }) }
        macro_rules! n { ($($expr:expr),+) => ({ Ngram::new(vec![$($expr),+]) }) }
        let sample =
            vec![n!(w!("a")), n!(w!("and")), n!(w!("by")), n!(w!("can")), n!(w!("git")), n!(w!("i")), n!(w!("is")), n!(w!("reason")),
                 n!(w!("rm")), n!(w!("simply")), n!(w!("submodule")), n!(w!("t")), n!(w!("the")), n!(w!("there")), n!(w!("way")),
                 n!(w!("whatever")), n!(p!("'")), n!(p!(",")), n!(p!("?")),

                 n!(w!("a"), w!("reason")),
                 n!(w!("and"), w!("by")),
                 n!(w!("by"), w!("the")),
                 n!(w!("can"), p!("'")),
                 n!(w!("git"), w!("submodule")),
                 n!(w!("i"), w!("can")),
                 n!(w!("is"), w!("there")),
                 n!(w!("reason"), w!("i")),
                 n!(w!("rm"), w!("whatever")),
                 n!(w!("simply"), w!("git")),
                 n!(w!("submodule"), w!("rm")),
                 n!(w!("t"), w!("simply")),
                 n!(w!("the"), w!("way")),
                 n!(w!("there"), w!("a")),
                 n!(w!("way"), p!(",")),
                 n!(w!("whatever"), p!("?")),
                 n!(p!("'"), w!("t")),
                 n!(p!(","), w!("is")),
                 n!(w!("a"), w!("reason"), w!("i")),
                 n!(w!("and"), w!("by"), w!("the")),
                 n!(w!("by"), w!("the"), w!("way")),
                 n!(w!("can"), p!("'"), w!("t")),
                 n!(w!("git"), w!("submodule"), w!("rm")),
                 n!(w!("i"), w!("can"), p!("'")),
                 n!(w!("is"), w!("there"), w!("a")),
                 n!(w!("reason"), w!("i"), w!("can")),
                 n!(w!("rm"), w!("whatever"), p!("?")),
                 n!(w!("simply"), w!("git"), w!("submodule")),
                 n!(w!("submodule"), w!("rm"), w!("whatever")),
                 n!(w!("t"), w!("simply"), w!("git")),
                 n!(w!("the"), w!("way"), p!(",")),
                 n!(w!("there"), w!("a"), w!("reason")),
                 n!(w!("way"), p!(","), w!("is")),
                 n!(p!("'"), w!("t"), w!("simply")),
                 n!(p!(","), w!("is"), w!("there"))];
        assert_eq!(ngrams, sample);
    }
}
