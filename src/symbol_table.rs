use crate::symbol::Symbol;
use anyhow::anyhow;
use elf::gabi::{STT_FILE, STT_SECTION};
use elf::CachedReadBytes;
use indexmap::IndexSet;
use itertools::Itertools;
use std::collections::HashMap;
use std::fmt::Display;
use std::fs::File;
use thiserror::Error;

pub struct SymbolDisplayer<'a>(&'a SymbolTable);

impl Display for SymbolDisplayer<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}",
            self.0
                .symbols
                .iter()
                .format_with("\n", |(name, symbol), g| {
                    g(&format_args!("{name}: {symbol}"))
                })
        ))
    }
}

#[derive(Debug, Copy, Clone, Error)]
#[error("Invalid symbol table")]
struct InvalidSymbolTable;

pub struct BreakpointDisplayer<'a>(&'a SymbolTable);

impl Display for BreakpointDisplayer<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.0.breakpoints.iter().format("\n")))
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    symbols: HashMap<String, Symbol>,
    breakpoints: IndexSet<String>,
    pub active: bool,
}

#[derive(Debug, Copy, Clone, Error)]
#[error("Invalid symbol name")]
pub struct InvalidSymbolName;

impl SymbolTable {
    pub fn new(symbols: HashMap<String, Symbol>) -> Self {
        Self {
            symbols,
            breakpoints: IndexSet::new(),
            active: true,
        }
    }

    pub fn read_from_file(path: &str) -> anyhow::Result<Self> {
        let mut file = elf::File::open_stream(CachedReadBytes::new(File::open(path)?))?;
        let (symtab, symstrtab) = file
            .symbol_table()?
            .ok_or_else(|| anyhow!("No symbol table in {}", path))?;
        let symbols = symtab
            .iter()
            .skip(1)
            .filter(|sym| sym.st_symtype().0 != STT_FILE && sym.st_symtype().0 != STT_SECTION)
            .map(|sym| {
                (
                    symstrtab.get(sym.st_name as usize).unwrap().to_string(),
                    Symbol::from(sym),
                )
            })
            .collect::<HashMap<_, _>>();
        Ok(Self::new(symbols))
    }

    pub fn update_symbols_from(&mut self, table: Self) {
        self.breakpoints = self
            .breakpoints
            .iter()
            .cloned()
            .filter(|sym| table.symbols.contains_key(sym))
            .collect::<IndexSet<_>>();
        self.symbols = table.symbols;
    }

    pub fn breakpoint_set_at(&self, addr: u32) -> bool {
        self.breakpoints
            .iter()
            .any(|sym| self.symbols[sym].value() == addr)
    }

    pub fn set_breakpoint(&mut self, symbol: String) {
        self.breakpoints.insert(symbol);
    }

    pub fn delete_breakpoint(&mut self, symbol: &str) -> bool {
        self.breakpoints.shift_remove(symbol)
    }

    pub fn address_to_symbol(&self, addr: u32) -> Option<(&String, u32)> {
        self.symbols
            .iter()
            .filter(|(_, sym)| sym.value() <= addr)
            .map(|(sym_name, sym)| (sym_name, addr - sym.value()))
            .min_by_key(|(_, offset)| *offset)
    }

    pub fn get_symbol(&self, symbol: &str) -> anyhow::Result<&Symbol> {
        Ok(self.symbols.get(symbol).ok_or(InvalidSymbolName)?)
    }

    pub fn contains_symbol(&self, symbol: &str) -> bool {
        self.symbols.contains_key(symbol)
    }

    pub fn symbol_displayer(&self) -> SymbolDisplayer<'_> {
        SymbolDisplayer(self)
    }

    pub fn breakpoint_displayer(&self) -> BreakpointDisplayer {
        BreakpointDisplayer(self)
    }
}
