extern crate tokenizer;

use std::{fmt, cmp};
use std::io::Read;
use std::sync::Arc;
use std::ops::Deref;
use std::iter::Iterator;
use std::collections::VecDeque;
use tokenizer::{Tokens, Token};

#[derive(Clone, Eq)]
pub struct Ngram {
    mem: Arc<Vec<Token>>,
    len: usize,
}

impl Ngram {
    pub fn new(tokens: Vec<Token>) -> Ngram {
        let len = tokens.len();
        Ngram { mem: Arc::new(tokens), len: len, }
    }

    fn derive(&self) -> Ngram {
        Ngram { mem: self.mem.clone(), len: self.len - 1, }
    }
}

impl std::hash::Hash for Ngram {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.deref().hash(state);
    }
}

impl cmp::PartialEq<Ngram> for Ngram {
    fn eq(&self, other: &Ngram) -> bool {
        self.deref().eq(other.deref())
    }
}

impl fmt::Debug for Ngram {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Ngram({:?})", &*self)
    }
}

impl Deref for Ngram {
    type Target = [Token];

    fn deref(&self) -> &[Token] {
        let len = self.len;
        &self.mem[.. len]
    }
}

pub trait Filter {
    fn accept(&mut self, ngram: &Ngram) -> bool;
}

#[derive(Debug)]
pub struct AcceptEverything;
impl Filter for AcceptEverything {
    fn accept(&mut self, _ngram: &Ngram) -> bool { true }
}

pub struct Ngrams<I, E, F> {
    state: IterState,
    filter: F,
    src: Tokens<I, E>,
    max_ngram: usize,
    window: VecDeque<Token>,
}

#[derive(Clone, Copy)]
enum IterCont {
    Continue,
    Finished,
}

#[derive(Clone)]
enum IterState {
    Fill,
    Permute { len: usize, cont: IterCont, gen_type: NgramGenType, },
    Depleted,
}

impl<I, E, F> Ngrams<I, E, F> {
    pub fn new(src: Tokens<I, E>, max_ngram: usize, filter: F) -> Ngrams<I, E, F> 
        where I: Iterator<Item = Result<char, E>>, F: Filter
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
pub enum NgramGenType {
    Base,
    Derived(Ngram),
}

#[derive(Debug)]
pub struct NgramGen {
    pub ngram: Ngram,
    pub gen_type: NgramGenType,
}

impl<I, E, F> Iterator for Ngrams<I, E, F> where I: Iterator<Item = Result<char, E>>, F: Filter {
    type Item = Result<NgramGen, E>;

    fn next(&mut self) -> Option<Result<NgramGen, E>> {
        loop {
            enum Trans {
                Keep,
                NextState(IterState),
                ValueReady(NgramGen, IterState),
            }

            let trans = match &self.state {
                &IterState::Depleted =>
                    return None,
                &IterState::Fill if self.window.len() >= self.max_ngram => 
                    Trans::NextState(IterState::Permute { len: self.window.len(), cont: IterCont::Continue, gen_type: NgramGenType::Base, }),
                &IterState::Fill => match self.src.next() {
                    Some(Ok(Token::Newline)) => 
                        Trans::NextState(IterState::Permute { len: self.window.len(), cont: IterCont::Continue, gen_type: NgramGenType::Base, }),
                    Some(Ok(Token::Whitespaces(..))) =>
                        Trans::Keep,
                    Some(Ok(token)) => {
                        self.window.push_back(token);
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
    
}
