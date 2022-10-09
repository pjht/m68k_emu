//! Example using Repl with a custom error type.

use reedline_repl_rs::clap::{ArgMatches, Command};
use reedline_repl_rs::Repl;
use std::fmt;

#[derive(Debug)]
enum CustomError {
    ReplError(reedline_repl_rs::Error),
    StringError(String),
}

impl From<reedline_repl_rs::Error> for CustomError {
    fn from(e: reedline_repl_rs::Error) -> Self {
        CustomError::ReplError(e)
    }
}

impl fmt::Display for CustomError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CustomError::ReplError(e) => write!(f, "REPL Error: {}", e),
            CustomError::StringError(s) => write!(f, "String Error: {}", s),
        }
    }
}

impl std::error::Error for CustomError {}

/// Do nothing, unsuccesfully
fn hello<T>(_args: ArgMatches, _context: &mut T) -> Result<Option<String>, CustomError> {
    Err(CustomError::StringError("Returning an error".to_string()))
}

fn main() -> Result<(), reedline_repl_rs::Error> {
    let mut repl = Repl::new(())
        .with_name("MyApp")
        .with_version("v0.1.0")
        .with_description("My very cool app")
        .with_command(
            Command::new("hello").about("Do nothing, unsuccessfully"),
            hello,
        );
    repl.run()
}
