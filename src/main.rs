#![feature(bigint_helper_methods)]

mod backplane;
mod card;
mod disas;
// mod error;
mod instruction;
mod location;
mod m68k;
mod peek;
mod ram;
mod rom;
mod storage;
mod symbol;
mod symbol_table;
mod symbol_tables;
mod term;
use crate::{
    backplane::Backplane,
    location::Location,
    m68k::{BusError, M68K},
};
use anyhow::anyhow;
use disas::DisassemblyError;
use indexmap::IndexSet;
use itertools::Itertools;
use parse_int::parse;
use peek::{PeekFormat, PeekSize};
use reedline_repl_rs::{
    clap::{builder::BoolishValueParser, Arg, ArgAction, Command},
    Repl,
};
use serde::Deserialize;
use serde_yaml::Mapping;
use std::{fs, path::Path, process};
use symbol_tables::SymbolTables;

#[derive(Deserialize, Debug)]
struct CardConfig<'a> {
    #[serde(rename = "type")]
    typ: &'a str,
    #[serde(flatten)]
    config: Mapping,
}

#[derive(Deserialize, Debug)]
struct EmuConfig<'a> {
    #[serde(borrow)]
    cards: Vec<CardConfig<'a>>,
    #[serde(borrow)]
    symbol_tables: Option<Vec<&'a str>>,
}

struct EmuState {
    cpu: M68K,
    symbol_tables: SymbolTables,
    address_breakpoints: IndexSet<u32>,
}

fn main() -> Result<(), anyhow::Error> {
    let config_str = fs::read_to_string("config.yaml")
        .map_err(|e| anyhow!("Could not read config file ({})", e))?;
    let config: EmuConfig = serde_yaml::from_str(&config_str)
        .map_err(|e| anyhow!("Could not parse config file ({})", e))?;
    let mut backplane = Backplane::new();
    for card in config.cards {
        backplane.add_card(card.typ, &card.config)?;
    }
    let mut symbol_tables = SymbolTables::new();
    if let Some(initial_tables) = config.symbol_tables {
        for path in initial_tables {
            symbol_tables.load_table(path, true).unwrap();
        }
    }
    Repl::<_, anyhow::Error>::new(EmuState {
        cpu: M68K::new(backplane),
        symbol_tables,
        address_breakpoints: IndexSet::new(),
    })
    .with_name("68KEmu")
    .with_version("0.1.0")
    .with_banner("68K Backplane Computer Emulator")
    .with_description("68K Backplane Computer Emulator")
    .with_command(
        Command::new("card")
            .trailing_var_arg(true)
            .arg(
                Arg::new("num")
                    .required(true)
                    .help("The card number to send the command to"),
            )
            .arg(
                Arg::new("args")
                    .required(true)
                    .multiple_values(true)
                    .takes_value(true),
            )
            .about("Send a command to a card"),
        |args, state| {
            let num = args.get_one::<String>("num").unwrap().parse::<u8>()?;
            state
                .cpu
                .bus_mut()
                .cards_mut()
                .get_mut(num as usize)
                .ok_or_else(|| anyhow!("Card {} does not exist", num))?
                .cmd(
                    &args
                        .get_many::<String>("args")
                        .unwrap()
                        .map(String::as_str)
                        .collect_vec(),
                )?;
            Ok(None)
        },
    )
    .with_command(
        Command::new("ls").about("List the cards in the system"),
        |_, state| {
            Ok(Some(
                state
                    .cpu
                    .bus_mut()
                    .cards()
                    .iter()
                    .enumerate()
                    .map(|(i, card)| format!("Card {i}: {card}"))
                    .join("\n"),
            ))
        },
    )
    .with_command(
        Command::new("regs").about("Show CPU registers"),
        |_, state| Ok(Some(format!("{}", state.cpu))),
    )
    .with_command(
        Command::new("step")
            .arg(
                Arg::new("count")
                    .takes_value(true)
                    .help("Count of instructions to step by. Defaults to 1"),
            )
            .arg(
                Arg::new("print_ins")
                    .long("print_ins")
                    .short('i')
                    .action(ArgAction::SetTrue)
                    .help("Print instructions"),
            )
            .arg(
                Arg::new("print_regs")
                    .long("print_regs")
                    .short('r')
                    .action(ArgAction::SetTrue)
                    .help("Print ending registers"),
            )
            .about("Step the CPU"),
        |args, state| {
            let count = parse::<u32>(args.get_one::<String>("count").map_or("1", String::as_str))?;
            let mut out = String::new();
            for _ in 0..count {
                if state.cpu.stopped {
                    out += &format!("CPU stopped at PC {:#x}\n", state.cpu.pc());
                    break;
                }
                if args.get_flag("print_ins") {
                    let pc = state.cpu.pc();
                    out += &disas_fmt(&mut state.cpu, pc, &state.symbol_tables).0;
                }
                state.cpu.step();
            }
            if args.get_flag("print_regs") {
                out += &format!("{}\n", state.cpu);
            }
            if out.is_empty() {
                Ok(None)
            } else {
                out.pop(); // Remove trailing newline
                Ok(Some(out))
            }
        },
    )
    .with_command(
        Command::new("run")
            .arg(Arg::new("stop_addr").takes_value(true).help(
                "Optional address to stop execution at. Works as a breakpoint only for this run",
            ))
            .arg(
                Arg::new("print_ins")
                    .long("print_ins")
                    .short('p')
                    .action(ArgAction::SetTrue)
                    .help("Print all executed instructions"),
            )
            .about("Run the CPU"),
        |args, state| {
            let mut out = String::new();
            while !state.cpu.stopped {
                let stop_addr = args
                    .get_one::<String>("stop_addr")
                    .map(|s| state.symbol_tables.parse_location_address(s))
                    .transpose()?;
                if stop_addr.map(|a| state.cpu.pc() == a).unwrap_or(false)
                    || state.symbol_tables.breakpoint_set_at(state.cpu.pc())
                    || state.address_breakpoints.contains(&state.cpu.pc())
                {
                    break;
                }
                if args.get_flag("print_ins") {
                    let pc = state.cpu.pc();
                    out += &disas_fmt(&mut state.cpu, pc, &state.symbol_tables).0;
                }
                state.cpu.step();
            }
            out += &format!("{}\n", state.cpu);
            let pc = state.cpu.pc();
            out += &disas_fmt(&mut state.cpu, pc, &state.symbol_tables).0;
            out.pop(); // Remove trailing newline
            Ok(Some(out))
        },
    )
    .with_command(
        Command::new("reset").about("Reset the cards and CPU, in that order"),
        |_, state| {
            for card in state.cpu.bus_mut().cards_mut() {
                card.reset();
            }
            state.cpu.reset();
            Ok(None)
        },
    )
    .with_command(
        Command::new("peek")
            .arg(Arg::new("count").short('c').takes_value(true))
            .arg(Arg::new("fmt").short('f').required(true).takes_value(true))
            .arg(Arg::new("addr").required(true))
            .about("Peek a memory address"),
        |args, state| {
            let fmt_str = args.get_one::<String>("fmt").unwrap();
            if fmt_str.len() != 2 {
                return Err(anyhow!("Peek format length must be 2"));
            }
            let fmt = PeekFormat::try_from(fmt_str.chars().next().unwrap())?;
            let size = PeekSize::try_from(fmt_str.chars().nth(1).unwrap())?;
            let count = parse::<u32>(args.get_one::<String>("count").map_or("1", String::as_str))?;
            let addr = state
                .symbol_tables
                .parse_location_address(args.get_one::<String>("addr").unwrap())?;
            let mut data = Vec::new();
            let bus = state.cpu.bus_mut();
            for i in 0..count {
                match size {
                    PeekSize::Byte => data.push(bus.read_byte(addr + i)? as u32),
                    PeekSize::Word => data.push(bus.read_word(addr + (i * 2))? as u32),
                    PeekSize::LongWord => data.push(
                        (bus.read_word(addr + (i * 4))? as u32) << 16
                            | (bus.read_word(addr + (i * 4) + 2)? as u32),
                    ),
                }
            }
            Ok(Some(
                data.chunks(size.chunk_size())
                    .enumerate()
                    .map(|(i, c)| {
                        format!(
                            "0x{:x}: ",
                            addr + (size.chunk_size() * size.byte_count() * i) as u32
                        ) + &c.iter().map(|d| fmt.format(*d, size)).join(" ")
                    })
                    .join("\n"),
            ))
        },
    )
    .with_command(
        Command::new("disas")
            .arg(Arg::new("addr").help("Address to start disassembly at. Defaults to current PC"))
            .arg(
                Arg::new("count")
                    .short('c')
                    .takes_value(true)
                    .help("Count of instructions to disassemble. Defaults to 1"),
            )
            .about("Disassemble a region of memory"),
        |args, state| {
            let mut addr = args
                .get_one::<String>("addr")
                .map_or(Ok(state.cpu.pc()), |s| {
                    state.symbol_tables.parse_location_address(s)
                })?;
            let count = parse::<u32>(args.get_one::<String>("count").map_or("1", String::as_str))?;
            let mut out = String::new();
            for _ in 0..count {
                let (fmt, res) = disas_fmt(&mut state.cpu, addr, &state.symbol_tables);
                out += &fmt;
                match res {
                    Ok(new_addr) => {
                        addr = new_addr;
                    }
                    Err(_) => {
                        break;
                    }
                }
            }
            out.pop(); // Remove trailing newline
            Ok(Some(out))
        },
    )
    .with_command(
        Command::new("sym")
            .arg(Arg::new("file").help("The ELF file to load symbols from"))
            .arg(
                Arg::new("append")
                    .long("append")
                    .short('a')
                    .action(ArgAction::SetTrue)
                    .requires("file")
                    .conflicts_with_all(&["delete", "active"])
                    .help("Append the file's symbols to the loaded list of symbols"),
            )
            .arg(
                Arg::new("delete")
                    .long("delete")
                    .short('d')
                    .action(ArgAction::SetTrue)
                    .requires("file")
                    .conflicts_with_all(&["append", "active"])
                    .help("Delete the symbol table instead of loading it"),
            )
            .arg(
                Arg::new("active")
                    .long("active")
                    .short('c')
                    .takes_value(true)
                    .value_parser(BoolishValueParser::new())
                    .requires("file")
                    .conflicts_with_all(&["append", "delete"]),
            )
            .about("Load symbols from an ELF file, or list symbols if no file provided"),
        |args, state| {
            if let Some(file_path) = args.get_one::<String>("file") {
                let table_name = Path::new(&file_path).file_name().unwrap().to_str().unwrap();
                if args.get_flag("delete") {
                    state.symbol_tables.delete(table_name)?;
                } else if let Some(&active) = args.get_one::<bool>("active") {
                    state.symbol_tables.set_active(table_name, active)?;
                } else {
                    state
                        .symbol_tables
                        .load_table(file_path, args.get_flag("append"))?;
                }
                Ok(None)
            } else if state.symbol_tables.is_empty() {
                Ok(Some("No symbols".to_string()))
            } else {
                Ok(Some(format!("{}", state.symbol_tables.symbol_displayer())))
            }
        },
    )
    .with_command(
        Command::new("resolve")
            .arg(
                Arg::new("location")
                    .help("The location to resolve")
                    .required(true),
            )
            .help("Resolve a location to an address"),
        |args, state| {
            let location = args.get_one::<String>("location").unwrap();
            Ok(Some(format!(
                "{}",
                state
                    .symbol_tables
                    .parse_location(location)?
                    .displayer(&state.symbol_tables)
            )))
        },
    )
    .with_command(
        Command::new("bp")
            .arg(Arg::new("location").help("The location to set a breakpoint at"))
            .arg(
                Arg::new("delete")
                    .long("delete")
                    .short('d')
                    .action(ArgAction::SetTrue)
                    .requires("location")
                    .help("Delete the breakpoint instead of setting it"),
            )
            .help("Set a breakpoint or list current breakpoints"),
        |args, state| {
            if let Some(location) = args.get_one::<String>("location") {
                let location = state.symbol_tables.parse_location(location)?;
                if args.get_flag("delete") {
                    let deleted = match location {
                        Location::Symbol((table, symbol)) => {
                            state.symbol_tables.delete_breakpoint(&table, &symbol)?
                        }
                        Location::Address(address) => {
                            state.address_breakpoints.shift_remove(&address)
                        }
                    };
                    if deleted {
                        Ok(None)
                    } else {
                        Ok(Some("No such breakpoint".to_string()))
                    }
                } else {
                    match location {
                        Location::Symbol((table, symbol)) => {
                            state.symbol_tables.set_breakpoint(&table, symbol)?;
                        }
                        Location::Address(address) => {
                            state.address_breakpoints.insert(address);
                        }
                    };
                    Ok(None)
                }
            } else {
                let mut out = String::new();
                out += &format!("{}", state.symbol_tables.breakpoint_displayer());
                if !state.address_breakpoints.is_empty() {
                    if !out.is_empty() {
                        out += "\n";
                    }
                    out += "Address breakpoints:\n";
                    out += &format!("{}", state.address_breakpoints.iter().format("\n"));
                }
                if out.is_empty() {
                    Ok(Some("No breakpoints".to_string()))
                } else {
                    Ok(Some(out))
                }
            }
        },
    )
    .with_command(
        Command::new("quit")
            .visible_alias("q")
            .visible_alias("exit")
            .about("Quit"),
        |_, _| process::exit(0),
    )
    // Visible aliases don't actually work, so fake it with hidden subcommands
    .with_command(Command::new("q").hide(true), |_, _| process::exit(0))
    .with_command(Command::new("exit").hide(true), |_, _| process::exit(0))
    .run()?;
    Ok(())
}

fn disas_fmt(
    cpu: &mut M68K,
    addr: u32,
    symbol_tables: &SymbolTables,
) -> (String, Result<u32, DisassemblyError<BusError>>) {
    let addr_fmt = if let Some((table, symbol, offset)) = symbol_tables.address_to_symbol(addr) {
        format!("{}:{} + {} (0x{:x})", table, symbol, offset, addr)
    } else {
        format!("0x{:x}", addr)
    };
    match cpu.disassemble(addr) {
        Ok((ins, new_addr)) => (format!("{}: {}\n", addr_fmt, ins), Ok(new_addr)),
        Err(e) => (format!("{}: {}\n", addr_fmt, e), Err(e)),
    }
}
