mod parser_trait;
mod grammar;
mod parser_impl;

use crate::ast::Proof;
pub type Error = grammar::Error;

pub fn parse(
    markdown: &markdown::mdast::Node,
) -> Result<Proof, Error> {
    grammar::proof::<parser_impl::Parser<_>>(markdown)
}
