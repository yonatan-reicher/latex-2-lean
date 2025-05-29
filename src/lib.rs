mod types;
mod lean;
mod precedence;
mod formating;
mod parsing;
mod to_lean;

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

pub mod prelude {
    use super::*;
    pub use types::*;
    pub use super::markdown_parse_options;
    pub use parsing::parse;
    pub use parsing::Error as ParseError;
    pub use formating::format;
    pub use to_lean::proof as to_lean;
}
