//! This module defines the `Parser` trait, which abstracts implementation
//! details of parsing.

pub trait Parser {
    type Out<T>;
    type Err;

    // It very easy to get confused with things like
    // `Result<(Option<T>, Vec<Warning>), Error>`, so we just wrap this type in
    // an associated type.
    fn error<T>(&self, error: Self::Err) -> Self::Out<T>;
    // fn warning<T>(&self, warning: Warning) -> Self::Out<T>;
    fn ret<T>(&self, value: T) -> Self::Out<T>;

    fn map<T, U>(this: Self::Out<T>, f: impl FnOnce(T) -> U) -> Self::Out<U>;
    fn and_then<T, U>(this: Self::Out<T>, f: impl FnOnce(T) -> Self::Out<U>) -> Self::Out<U>;

    fn pop_keyword(&mut self, keyword: &str) -> bool;
    fn pop_name(&mut self) -> Option<String>;
    type ClosingBracket;
    fn pop_bracket(&mut self) -> Option<Self::ClosingBracket>;
    fn pop_closing_bracket(&mut self, b: Self::ClosingBracket) -> bool;
    // TODO: Make this non-mut
    fn eof(&mut self) -> bool;
}

