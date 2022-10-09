//! Example using Repl with a custom error type.
use reedline_repl_rs::clap::{Arg, ArgMatches, Command};
use reedline_repl_rs::{Repl, Result};

/// Write "Hello" with given name
async fn hello<T>(args: ArgMatches, _context: &mut T) -> Result<Option<String>> {
    Ok(Some(format!("Hello, {}", args.value_of("who").unwrap())))
}

/// Called after successful command execution, updates prompt with returned Option
async fn update_prompt<T>(_context: &mut T) -> Result<Option<String>> {
    Ok(Some("updated".to_string()))
}

#[tokio::main]
async fn main() -> Result<()> {
    let mut repl = Repl::new(())
        .with_name("MyApp")
        .with_version("v0.1.0")
        .with_command_async(
            Command::new("hello")
                .arg(Arg::new("who").required(true))
                .about("Greetings!"),
            |args, context| Box::pin(hello(args, context)),
        )
        .with_on_after_command_async(|context| Box::pin(update_prompt(context)));
    repl.run_async().await
}
