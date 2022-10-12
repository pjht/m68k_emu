#![feature(bigint_helper_methods)]

mod backplane;
mod card;
mod disas;
mod instruction;
mod m68k;
mod ram;
mod rom;
mod storage;
mod term;
use crate::{
    backplane::Backplane,
    m68k::{BusError, M68K},
};
use disas::DisassemblyError;
use elf::symbol::Symbol;
use itertools::Itertools;
use parse_int::parse;
use reedline_repl_rs::{
    clap::{Arg, ArgAction, Command},
    Error as ReplError, Repl,
};
use serde_yaml::Mapping;
use std::{convert::TryFrom, error, fmt::Display, fs, num::ParseIntError, process};

#[derive(Debug)]
enum Error {
    Repl(ReplError),
    InvalidCard(u8),
    Bus(BusError),
    InvalidPeekFormat,
    InvalidPeekSize,
    Disassembly(DisassemblyError<BusError>),
    Misc(&'static str),
    MiscDyn(Box<dyn error::Error>),
}

impl From<Box<dyn error::Error>> for Error {
    fn from(v: Box<dyn error::Error>) -> Self {
        Self::MiscDyn(v)
    }
}

impl From<DisassemblyError<BusError>> for Error {
    fn from(v: DisassemblyError<BusError>) -> Self {
        Self::Disassembly(v)
    }
}

impl From<BusError> for Error {
    fn from(v: BusError) -> Self {
        Self::Bus(v)
    }
}

impl From<ReplError> for Error {
    fn from(v: ReplError) -> Self {
        Self::Repl(v)
    }
}

impl From<ParseIntError> for Error {
    fn from(v: ParseIntError) -> Self {
        Self::Repl(v.into())
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Repl(e) => e.fmt(f),
            Self::InvalidCard(n) => f.write_fmt(format_args!("Card {} does not exist", n)),
            Self::Bus(e) => e.fmt(f),
            Self::InvalidPeekFormat => f.write_str("Invalid peek format"),
            Self::InvalidPeekSize => f.write_str("Invalid peek size"),
            Self::Disassembly(e) => e.fmt(f),
            Self::Misc(s) => f.write_str(s),
            Self::MiscDyn(e) => e.fmt(f),
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum PeekFormat {
    Octal,
    Hex,
    Decimal,
    UnsignedDecimal,
    Binary,
}

impl PeekFormat {
    pub fn format(self, num: u32, size: PeekSize) -> String {
        match self {
            Self::Octal => format!("Oo{:0>width$o}", num, width = size.byte_count()),
            Self::Hex => format!("0x{:0>width$x}", num, width = size.byte_count() * 2),
            Self::Decimal => {
                let num = match size {
                    PeekSize::Byte => num as u8 as i8 as i32,
                    PeekSize::Word => num as u16 as i16 as i32,
                    PeekSize::LongWord => num as i32,
                };
                format!("{}", num)
            }
            Self::UnsignedDecimal => format!("{}", num),
            Self::Binary => format!("0b{:0>width$b}", num, width = size.byte_count() * 8),
        }
    }
}

impl TryFrom<char> for PeekFormat {
    type Error = Error;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'o' => Ok(Self::Octal),
            'x' => Ok(Self::Hex),
            'd' => Ok(Self::Decimal),
            'u' => Ok(Self::UnsignedDecimal),
            'b' => Ok(Self::Binary),
            _ => Err(Error::InvalidPeekFormat),
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum PeekSize {
    Byte,
    Word,
    LongWord,
}

impl PeekSize {
    fn byte_count(self) -> usize {
        match self {
            PeekSize::Byte => 1,
            PeekSize::Word => 2,
            PeekSize::LongWord => 4,
        }
    }

    fn chunk_size(self) -> usize {
        match self {
            PeekSize::Byte => 8,
            PeekSize::Word => 8,
            PeekSize::LongWord => 4,
        }
    }
}

impl TryFrom<char> for PeekSize {
    type Error = Error;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'b' => Ok(Self::Byte),
            'w' => Ok(Self::Word),
            'l' => Ok(Self::LongWord),
            _ => Err(Error::InvalidPeekSize),
        }
    }
}

struct EmuState {
    cpu: M68K,
    symbols: Vec<Symbol>,
}

fn main() -> Result<(), ReplError> {
    let config: Mapping = serde_yaml::from_str(
        &fs::read_to_string("config.yaml").expect("Could not read config file"),
    )
    .expect("Could not parse config file");
    let mut backplane = Backplane::new();
    for card in config
        .get("cards")
        .expect("Could not get cards config info")
        .as_sequence()
        .expect("Cards config is not list")
    {
        let card = card.as_mapping().expect("Card config not mapping");
        let typ = card
            .get("type")
            .expect("Card config has no type")
            .as_str()
            .expect("Card type is not string");
        match backplane.add_card(typ, card) {
            Ok(_) => (),
            Err(e) => panic!("{}", e),
        };
    }
    Repl::<_, Error>::new(EmuState {
        cpu: M68K::new(backplane),
        symbols: Vec::new(),
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
                .ok_or(Error::InvalidCard(num))?
                .cmd(
                    &args
                        .get_many::<String>("args")
                        .unwrap()
                        .map(String::as_str)
                        .collect_vec(),
                );
            Ok(None)
        },
    )
    .with_command(
        Command::new("ls").about("List the cards in the system"),
        |_, state| {
            #[allow(unstable_name_collisions)]
            Ok(Some(
                state
                    .cpu
                    .bus_mut()
                    .cards()
                    .iter()
                    .enumerate()
                    .map(|(i, card)| format!("Card {i}: {card}"))
                    .intersperse('\n'.to_string())
                    .collect(),
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
                    out += &disas_fmt(&mut state.cpu, pc).0;
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
                    .map(|s| parse_addr(s, &state.symbols))
                    .transpose()?;
                if stop_addr.map(|a| state.cpu.pc() == a).unwrap_or(false) {
                    break;
                }
                if args.get_flag("print_ins") {
                    let pc = state.cpu.pc();
                    out += &disas_fmt(&mut state.cpu, pc).0;
                }
                state.cpu.step();
            }
            out += &format!("{}\n", state.cpu);
            let pc = state.cpu.pc();
            out += &disas_fmt(&mut state.cpu, pc).0;
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
                return Err(Error::Misc("Peek format length must be 2"));
            }
            let fmt = PeekFormat::try_from(fmt_str.chars().next().unwrap())?;
            let size = PeekSize::try_from(fmt_str.chars().nth(1).unwrap())?;
            let count = parse::<u32>(args.get_one::<String>("count").map_or("1", String::as_str))?;
            let addr = parse_addr(args.get_one::<String>("addr").unwrap(), &state.symbols)?;
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
            #[allow(unstable_name_collisions)]
            Ok(Some(
                data.chunks(size.chunk_size())
                    .enumerate()
                    .map(|(i, c)| {
                        format!(
                            "0x{:x}: ",
                            addr + (size.chunk_size() * size.byte_count() * i) as u32
                        ) + &c
                            .iter()
                            .map(|d| fmt.format(*d, size))
                            .intersperse(" ".to_string())
                            .collect::<String>()
                    })
                    .intersperse("\n".to_string())
                    .collect::<String>(),
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
                .map_or(Ok(state.cpu.pc()), |s| parse_addr(s, &state.symbols))?;
            let count = parse::<u32>(args.get_one::<String>("count").map_or("1", String::as_str))?;
            let mut out = String::new();
            for _ in 0..count {
                let (fmt, res) = disas_fmt(&mut state.cpu, addr);
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
            .arg(
                Arg::new("file")
                    .required(true)
                    .help("The ELF file to load symbols from"),
            )
            .arg(
                Arg::new("append")
                    .long("append")
                    .short('a')
                    .action(ArgAction::SetTrue)
                    .help("Append the file's symbols to the loaded list of symbols"),
            )
            .about("Load symbols from an ELF file"),
        |args, state| {
            let file = args.get_one::<String>("file").unwrap();
            let file = elf::File::open_path(file).map_err(<Box<dyn error::Error>>::from)?;
            let symtab = file
                .get_section(".symtab")
                .ok_or(Error::Misc("Could not find symbol table section"))?;
            let symbols = file
                .get_symbols(&symtab)
                .map_err(<Box<dyn error::Error>>::from)?;
            if args.get_flag("append") {
                state.symbols.extend_from_slice(&symbols[..]);
            } else {
                state.symbols = symbols;
            }
            Ok(None)
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
    .run()
}

fn disas_fmt(cpu: &mut M68K, addr: u32) -> (String, Result<u32, DisassemblyError<BusError>>) {
    match cpu.disassemble(addr) {
        Ok((ins, new_addr)) => (format!("0x{:x}: {}\n", addr, ins), Ok(new_addr)),
        Err(e) => (format!("0x{:x}: {}\n", addr, e), Err(e)),
    }
}

fn parse_addr(addr: &str, symbols: &[Symbol]) -> Result<u32, Error> {
    parse::<u32>(addr).or_else(|_| {
        symbols
            .iter()
            .find(|sym| sym.name == addr)
            .map(|sym| sym.value as u32)
            .ok_or(Error::Misc("No such symbol"))
    })
}
