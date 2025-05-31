use latex_2_lean::*;
use std::io;
use thiserror::Error;

const HELP_MSG: &str = r#"
latex-2-lean - Converting LaTeX definitions from markdown files to lean code.

Usage: latex-2-lean <file>
"#
.trim_ascii();

#[derive(Debug, Clone, Error)]
#[error("{message}")]
struct MarkdownError {
    message: markdown::message::Message,
}
impl MarkdownError {
    fn new(message: markdown::message::Message) -> Self {
        Self { message }
    }
}

#[derive(Debug, Error)]
enum Error {
    #[error("Invalid arguments. Must have exactly one argument: the file path.")]
    InvalidArguments,
    #[error("Failed to read the input file ({0})")]
    FailedToReadInputFile(#[from] io::Error),
    #[error("Parse error: {0}")]
    Parse(#[from] ParseError),
    #[error("Markdown error: {0}")]
    Markdown(#[from] MarkdownError),
}

fn get_file_path() -> Result<String, Error> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        return Err(Error::InvalidArguments);
    }
    Ok(args[1].clone())
}

fn try_except<T>(
    action: impl FnOnce() -> Result<T, Error>,
    handler: impl FnOnce(Error) -> T,
) -> T {
    action().unwrap_or_else(handler)
}

fn main() {
    try_except(|| {
        let file_path = get_file_path()?;
        let text = std::fs::read_to_string(file_path)?;
        let ast = markdown::to_mdast(
            &text,
            &markdown::ParseOptions {
                constructs: markdown::Constructs {
                    math_flow: true,
                    math_text: true,
                    ..markdown::ParseOptions::gfm().constructs
                },
                ..markdown::ParseOptions::gfm()
            },
        ).map_err(MarkdownError::new)?;
        let proof = parse(&ast)?;
        println!("Input:\n{:#?}", proof);
        // println!("Pretty:\n{}", format(&proof));
        // check_program(&proof)?;
        // println!("Proof is valid!");
        Ok(())
    }, |err| {
        eprintln!("ERROR: {}\n", err);
        eprintln!("{}", HELP_MSG);
    })
}
