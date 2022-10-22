use crate::{disas::DisassemblyError, m68k::BusError};
use reedline_repl_rs::Error as ReplError;
use std::{error, fmt::Display, io, num::ParseIntError};

#[derive(Debug)]
pub enum Error {
    Repl(ReplError),
    InvalidCard(u8),
    Bus(BusError),
    InvalidPeekFormat,
    InvalidPeekSize,
    Disassembly(DisassemblyError<BusError>),
    Misc(&'static str),
    InvalidSymbolTable,
    InvalidSymbolName,
    Io(io::Error),
    ElfParse(elf::ParseError),
}

impl From<elf::ParseError> for Error {
    fn from(v: elf::ParseError) -> Self {
        Self::ElfParse(v)
    }
}

impl From<io::Error> for Error {
    fn from(v: io::Error) -> Self {
        Self::Io(v)
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
            Self::InvalidSymbolTable => f.write_str("Invalid symbol table"),
            Self::InvalidSymbolName => f.write_str("Invalid symbol name"),
            Self::Io(e) => e.fmt(f),
            Self::ElfParse(e) => e.fmt(f),
        }
    }
}
