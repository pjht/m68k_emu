//! Example using Repl without Context (or, more precisely, a Context of ())
use reedline_repl_rs::clap::{Arg, ArgMatches, Command};
use reedline_repl_rs::{Repl, Result};

/// Add two numbers. Have to make this generic to be able to pass a Context of type ()
fn add<T>(args: ArgMatches, _context: &mut T) -> Result<Option<String>> {
    let first: i32 = args.value_of("first").unwrap().parse()?;
    let second: i32 = args.value_of("second").unwrap().parse()?;

    Ok(Some((first + second).to_string()))
}

/// Write "Hello"
fn hello<T>(args: ArgMatches, _context: &mut T) -> Result<Option<String>> {
    Ok(Some(format!("Hello, {}", args.value_of("who").unwrap())))
}

fn main() -> Result<()> {
    let mut repl = Repl::new(())
        .with_name("MyApp")
        .with_version("v0.1.0")
        .with_description("My very cool app")
        .with_command(
            Command::new("add")
                .arg(Arg::new("first").required(true))
                .arg(Arg::new("second").required(true))
                .about("Add two numbers together"),
            add,
        )
        .with_command(
            Command::new("hello")
                .arg(Arg::new("who").required(true))
                .about("Greetings!"),
            hello,
        );
    repl.run()
}
