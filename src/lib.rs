// Types
mod ast;
mod lean;
mod precedence;
// Things we do with types
mod formating;
mod parsing;
mod ast_to_lean;
mod lean_to_string;

pub fn markdown_parse_options() -> markdown::ParseOptions {
    use markdown::{Constructs, ParseOptions};
    ParseOptions {
        constructs: Constructs {
            math_flow: true,
            math_text: true,
            ..ParseOptions::gfm().constructs
        },
        ..ParseOptions::gfm()
    }
}

// Types
pub use ast::*;
pub use parsing::Error as ParseError;
// Functionality
pub use formating::format;
pub use parsing::parse;
pub use ast_to_lean::proof as ast_to_lean;
pub use lean_to_string::program as lean_to_string;
