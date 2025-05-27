//! This module defines the grammar of the language.

use super::parser_trait::Parser;
use crate::precedence::Precedence;
use crate::types::{Definition, Proof, Term, SetComprehension};
use markdown::mdast::Node;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("A definition must start with a name (e.g. `A = ...`)")]
    NameExpected,
    #[error("A definition must have an equal sign (e.g. `A = ...`)")]
    EqualSignExpected,
    #[error("Expected a closing bracket for the set")]
    EndOfSetExpected,
}

#[derive(Debug)]
pub enum Warning {}

fn set_term<'input, P: Parser<'input, Err = Error>>(p: &mut P) -> Option<P::Out<Term>> {
    if !p.pop_symbol("\\{") { return None; }
    if p.pop_symbol("\\}") { return Some(p.ret(Term::Set(vec![]))); }
    
    let t1 = term(p);
    if p.pop_symbol("\\mid") {
        // This is a set comprehension term
        let cond = term(p);
        if !p.pop_symbol("\\}") {
            return Some(p.error(Error::EndOfSetExpected));
        }
        return Some(P::and_then(t1, |t1| {
            P::map(cond, |cond| Term::SetComprehension(SetComprehension::Mapping {
                expression: Box::new(t1),
                condition: Box::new(cond),
            }) )
        }));
    }

    // This is a set term which is not a comprehension

    let mut terms = P::map(t1, |t1| vec![t1]);
    while p.pop_symbol(",") {
        let t = term(p);
        terms = P::and_then(terms, |mut terms| {
            P::map(t, |t| {
                terms.push(t);
                terms
            })
        })
    }
    if !p.pop_symbol("\\}") {
        return Some(p.error(Error::EndOfSetExpected));
    }
    Some(P::map(terms, Term::Set))
}

fn term<'input, P: Parser<'input, Err = Error>>(p: &mut P) -> P::Out<Term> {
    if let Some(t) = set_term(p) { return t; }
    if let Some(name) = p.pop_name() {
        return p.ret(Term::Var(name.to_string()));
    }
    todo!()
}

fn definition<'input, P: Parser<'input, Err = Error>>(p: &mut P) -> P::Out<Definition> {
    let Some(name) = p.pop_name() else {
        return p.error(Error::NameExpected);
    };

    if !p.pop_symbol("=") {
        return p.error(Error::EqualSignExpected);
    }

    P::map(term(p), |term| Definition {
        name: name.to_string(),
        term,
    })
}

fn definition_node<'input, P: Parser<'input, Err = Error>>(
    node: &'input Node,
) -> Option<P::Out<Definition>> {
    let (latex_text, position) = match node {
        Node::InlineMath(inline_math) => (&inline_math.value, inline_math.position.as_ref()),
        Node::Math(math) => (&math.value, math.position.as_ref()),
        _ => return None,
    };

    let position = position.expect("Node must have a position");
    let mut parser = P::new(latex_text, position.clone());
    let result = definition(&mut parser);
    // TODO: Make sure that the parser has reached the end of the input
    Some(result)
}

fn children_recursive<'input>(
    markdown: &'input Node,
) -> Box<dyn Iterator<Item = &'input Node> + 'input> {
    let iter = markdown
        .children()
        .into_iter()
        .flat_map(|v| v.iter())
        .flat_map(move |child| std::iter::once(child).chain(children_recursive(child)));
    Box::new(iter)
}

pub fn proof<'input, P: Parser<'input, Err = Error>>(markdown: &'input Node) -> P::Out<Proof> {
    let definitions = children_recursive(markdown)
        .filter_map(definition_node::<'input, P>)
        // Now we have an iterator of `P::Out<Definition>`
        .fold(None, |acc, def| match acc {
            None => Some(P::map(def, |d| vec![d])),
            Some(defs) => Some(P::and_then(defs, |mut defs| {
                P::map(def, |d| {
                    defs.push(d);
                    defs
                })
            })),
        });
    P::map(
        definitions.expect("No definitions in the program!"),
        |definitions| Proof { definitions },
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    /*
    #[test]
    fn forall_implies_forall() {
        enum TestParser {
            Forall1,
            X1,
            Y1,
            Implies,
            Forall2,
            X2,
            Y2,
            Eof,
        }
        use TestParser::*;

        impl Parser for TestParser {
            type Out<T> = Option<T>;
            type Err = Error;

            fn error<T>(&self, _: Error) -> Self::Out<T> {
                None
            }
            // fn warning<T>(&self, _: Warning) -> Self::Out<T> {
            //     None
            // }
            fn ret<T>(&self, value: T) -> Self::Out<T> {
                Some(value)
            }
            fn map<T, U>(this: Self::Out<T>, f: impl FnOnce(T) -> U) -> Self::Out<U> {
                this.map(f)
            }
            fn and_then<T, U>(
                this: Self::Out<T>,
                f: impl FnOnce(T) -> Self::Out<U>,
            ) -> Self::Out<U> {
                this.and_then(f)
            }
            fn pop_keyword(&mut self, keyword: &str) -> bool {
                match self {
                    Forall1 => {
                        keyword == "\\forall" && {
                            *self = TestParser::X1;
                            true
                        }
                    }
                    Forall2 => {
                        keyword == "\\forall" && {
                            *self = TestParser::X2;
                            true
                        }
                    }
                    Implies => {
                        keyword == "\\implies" && {
                            *self = TestParser::Forall2;
                            true
                        }
                    }
                    _ => false,
                }
            }
            fn pop_name(&mut self) -> Option<String> {
                match self {
                    X1 => {
                        *self = TestParser::Y1;
                        Some("x".to_string())
                    }
                    Y1 => {
                        *self = TestParser::Implies;
                        Some("y".to_string())
                    }
                    X2 => {
                        *self = TestParser::Y2;
                        Some("x".to_string())
                    }
                    Y2 => {
                        *self = TestParser::Eof;
                        Some("y".to_string())
                    }
                    _ => None,
                }
            }
            fn eof(&mut self) -> bool {
                matches!(self, Eof)
            }

            type ClosingBracket = ();

            fn pop_bracket(&mut self) -> Option<Self::ClosingBracket> {
                None
            }

            fn pop_closing_bracket(&mut self, _: Self::ClosingBracket) -> bool {
                unreachable!("No brackets in this test parser")
            }
        }

        let mut parser = TestParser::Forall1;
        let result = prop(&mut parser).unwrap();
        assert_eq!(
            result,
            Prop::Implies(
                Box::new(Prop::Forall(
                    "x".to_string(),
                    Box::new(Prop::Var("y".to_string()))
                )),
                Box::new(Prop::Forall(
                    "x".to_string(),
                    Box::new(Prop::Var("y".to_string()))
                ))
            )
        );
    }
    */
}
