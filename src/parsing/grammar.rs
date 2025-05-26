//! This module defines the grammar of the language.

use super::parser_trait::Parser;
use crate::precedence::Precedence;
use crate::types::{Program, Prop, Statement};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Bracket mismatch")]
    BracketMismatch,
    #[error("Expected a proposition here")]
    ExpecetedAPropositionHere,
    #[error("Expected a name after quantifier")]
    ExpecetedNameAfterQuantifier,
    #[error("Expected a statement. A statement must start with either `Assume` or `Assert`")]
    BadStatementStart,
}

#[derive(Debug)]
pub enum Warning {}

// TODO: Use macro_rules to fight the boilerplate
fn prop_prec<P: Parser<Err = Error>>(input: &mut P, precedence: Precedence) -> P::Out<Prop> {
    use Prop::*;

    // Brackets ( [ {
    if let Some(b) = input.pop_bracket() {
        let inner = prop_prec(input, Precedence::min());
        if !input.pop_closing_bracket(b) {
            return input.error(Error::BracketMismatch);
        }
        return inner;
    }

    // Variables
    if let Precedence::Var = precedence {
        let Some(name) = input.pop_name() else {
            return input.error(Error::ExpecetedAPropositionHere);
        };
        return input.ret(Var(name));
    }

    if precedence == Precedence::Quantifier && input.pop_keyword("\\forall") {
        let Some(name) = input.pop_name() else {
            return input.error(Error::ExpecetedNameAfterQuantifier);
        };
        let inner = prop_prec(input, Precedence::Quantifier.inc());
        return P::map(inner, |inner| Forall(name, Box::new(inner)));
    }

    P::and_then(
        // First parse the left side of some operator
        prop_prec(input, precedence.inc()),
        |left| {
            if precedence == Precedence::Implies && input.pop_keyword("\\implies") {
                return P::and_then(prop_prec(input, precedence.inc()), |right| {
                    input.ret(Implies(Box::new(left), Box::new(right)))
                });
            }
            // Operator was not found! Return the lhs as the expression
            input.ret(left)
        },
    )
}

pub fn prop<P: Parser<Err = Error>>(input: &mut P) -> P::Out<Prop> {
    prop_prec(input, Precedence::min())
}

pub fn statement<P: Parser<Err = Error>>(p: &mut P) -> P::Out<Statement> {
    // TODO: The inner `prop` should b wrapped in $$ to resemble latex syntax.
    if p.pop_keyword("Assert") {
        let inner = prop(p);
        return P::map(inner, Statement::Assert);
    }
    if p.pop_keyword("Assume") {
        let inner = prop(p);
        return P::map(inner, Statement::Assume);
    }
    p.error(Error::BadStatementStart)
}

fn statements<P: Parser<Err = Error>>(
    p: &mut P,
    mut acc: Vec<Statement>,
) -> P::Out<Vec<Statement>> {
    if p.eof() {
        return p.ret(acc);
    }
    let s = statement(p);
    P::and_then(s, |s| {
        acc.push(s);
        statements(p, acc)
    })
}

pub fn program<P: Parser<Err = Error>>(p: &mut P) -> P::Out<Program> {
    let s = statements(p, vec![]);
    P::map(s, |s| Program { statements: s })
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
