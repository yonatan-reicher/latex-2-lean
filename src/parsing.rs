mod grammar;
mod parser_impl;
mod parser_trait;

use crate::types::Program;
pub type Error = grammar::Error;

pub fn parse(text: &str) -> Result<Program, Error> {
    grammar::program(&mut parser_impl::Parser::new(text))
}
