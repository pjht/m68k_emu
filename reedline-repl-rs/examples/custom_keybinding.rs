//! Example with custom Keybinding
use crossterm::event::{KeyCode, KeyModifiers};
use reedline::{EditCommand, ReedlineEvent};
use reedline_repl_rs::clap::{Arg, ArgMatches, Command};
use reedline_repl_rs::{Repl, Result};

/// Write "Hello" with given name
fn hello<T>(args: ArgMatches, _context: &mut T) -> Result<Option<String>> {
    Ok(Some(format!("Hello, {}", args.value_of("who").unwrap())))
}

fn main() -> Result<()> {
    let mut repl = Repl::new(())
        .with_name("MyApp")
        .with_version("v0.1.0")
        .with_description("My very cool app")
        .with_banner("Welcome to MyApp")
        .with_command(
            Command::new("hello")
                .arg(Arg::new("who").required(true))
                .about("Greetings!"),
            hello,
        )
        // greet friend with CTRG+g
        .with_keybinding(
            KeyModifiers::CONTROL,
            KeyCode::Char('g'),
            ReedlineEvent::ExecuteHostCommand("hello Friend".to_string()),
        )
        // show help with CTRL+h
        .with_keybinding(
            KeyModifiers::CONTROL,
            KeyCode::Char('h'),
            ReedlineEvent::ExecuteHostCommand("help".to_string()),
        )
        // uppercase current word with CTRL+u
        .with_keybinding(
            KeyModifiers::CONTROL,
            KeyCode::Char('u'),
            ReedlineEvent::Edit(vec![EditCommand::UppercaseWord]),
        )
        // uppercase current word with CTRL+l
        .with_keybinding(
            KeyModifiers::CONTROL,
            KeyCode::Char('l'),
            ReedlineEvent::Edit(vec![EditCommand::LowercaseWord]),
        );

    println!("Keybindings:");
    let keybindings = repl.get_keybindings();
    for search_modifier in [
        KeyModifiers::NONE,
        KeyModifiers::CONTROL,
        KeyModifiers::SHIFT,
        KeyModifiers::ALT,
    ] {
        for ((modifier, key_code), reedline_event) in &keybindings {
            if *modifier == search_modifier {
                println!("{:?} + {:?} => {:?}", modifier, key_code, reedline_event);
            }
        }
    }
    repl.run()
}
