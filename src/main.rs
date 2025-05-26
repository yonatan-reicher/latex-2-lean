use latex_2_lean::prelude::*;
use std::io;
use thiserror::Error;

const HELP_MSG: &str = r#"
Proofy - A simple proof assistant with markdown+latex compatible syntax.

Usage: proofy <file>
"#
.trim_ascii();

#[derive(Debug, Error)]
enum Error {
    #[error("Invalid arguments. Must have exactly one argument: the file path.")]
    InvalidArguments,
    #[error("Failed to read the input file ({0})")]
    FailedToReadInputFile(#[from] io::Error),
    #[error("Parse error: {0}")]
    Parse(#[from] ParseError),
    #[error("Proof error: {0}")]
    Proof(#[from] proofy::prelude::Error),
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
        let prog = parse(&text)?;
        println!("Input:\n{:#?}", prog);
        println!("Pretty:\n{}", format_program(&prog));
        check_program(&prog)?;
        println!("Proof is valid!");
        Ok(())
    }, |err| {
        eprintln!("ERROR: {}\n", err);
        eprintln!("{}", HELP_MSG);
    })
}
