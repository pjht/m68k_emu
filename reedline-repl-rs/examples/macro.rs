//! Example using initialize_repl macro
use reedline_repl_rs::clap::{Arg, ArgMatches, Command};
use reedline_repl_rs::{initialize_repl, Repl, Result};

/// Write "Hello" with given name
fn hello<T>(args: ArgMatches, _context: &mut T) -> Result<Option<String>> {
    Ok(Some(format!("Hello, {}", args.value_of("who").unwrap())))
}

fn main() -> Result<()> {
    let mut repl = initialize_repl!(()).with_command(
        Command::new("hello")
            .arg(Arg::new("who").required(true))
            .about("Greetings!"),
        hello,
    );
    repl.run()
}
