use crate::command::ReplCommand;
use clap::Command;
use reedline::{Completer, Span, Suggestion};
use std::collections::HashMap;

pub(crate) struct ReplCompleter {
    commands: HashMap<String, Command<'static>>,
}

impl Completer for ReplCompleter {
    fn complete(&mut self, line: &str, pos: usize) -> Vec<Suggestion> {
        let mut completions = vec![];
        completions.extend(if line.contains(' ') {
            let mut words = line[0..pos].split(' ');
            let first_word = words.next().unwrap();
            let mut words_rev = words.rev();
            if let Some(command) = self.commands.get(first_word) {
                let last_word = words_rev.next().unwrap();
                let last_word_start_pos = line.len() - last_word.len();
                let span = Span::new(last_word_start_pos, pos);
                self.parameter_values_starting_with(command, words_rev.count(), last_word, span)
            } else {
                vec![]
            }
        } else {
            let span = Span::new(0, pos);
            self.commands_starting_with(line, span)
        });
        completions.dedup();
        completions
    }
}

impl ReplCompleter {
    pub fn new<Context, E>(repl_commands: &HashMap<String, ReplCommand<Context, E>>) -> Self {
        let mut commands = HashMap::new();
        for (name, repl_command) in repl_commands.iter() {
            commands.insert(name.clone(), repl_command.command.clone());
        }
        ReplCompleter { commands }
    }

    fn build_suggestion(&self, value: &str, help: Option<&str>, span: Span) -> Suggestion {
        Suggestion {
            value: value.to_string(),
            description: help.map(|n| n.to_string()),
            extra: None,
            span,
            append_whitespace: true,
        }
    }

    fn parameter_values_starting_with(
        &self,
        command: &Command<'static>,
        _parameter_idx: usize,
        search: &str,
        span: Span,
    ) -> Vec<Suggestion> {
        let mut completions = vec![];
        for arg in command.get_arguments() {
            // skips --help and --version
            if arg.is_global_set() {
                continue;
            }
            if let Some(possible_values) = arg.get_possible_values() {
                completions.extend(
                    possible_values
                        .iter()
                        .filter(|value| value.get_name().starts_with(search))
                        .map(|value| {
                            self.build_suggestion(value.get_name(), value.get_help(), span)
                        }),
                );
            }

            if let Some(long) = arg.get_long() {
                let value = "--".to_string() + long;
                if value.starts_with(search) {
                    completions.push(self.build_suggestion(&value, arg.get_help(), span));
                }
            }

            if let Some(short) = arg.get_short() {
                let value = "-".to_string() + &short.to_string();
                if value.starts_with(search) {
                    completions.push(self.build_suggestion(&value, arg.get_help(), span));
                }
            }
        }
        completions
    }

    fn commands_starting_with(&self, search: &str, span: Span) -> Vec<Suggestion> {
        let mut result: Vec<Suggestion> = self
            .commands
            .iter()
            .filter(|(key, _)| key.starts_with(search))
            .map(|(_, command)| {
                self.build_suggestion(command.get_name(), command.get_about(), span)
            })
            .collect();

        if "help".starts_with(search) {
            result.push(self.build_suggestion("help", Some("show help"), span));
        }

        result
    }
}
