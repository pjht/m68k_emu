use crate::{error::Error, symbol::Symbol};
use elf::gabi::{STT_FILE, STT_SECTION};
use indexmap::IndexSet;
use std::collections::HashMap;
use std::error::Error as StdError;
use std::fs::File;

#[derive(Debug)]
pub struct SymbolTable {
    pub symbols: HashMap<String, Symbol>,
    pub breakpoints: IndexSet<String>,
    pub active: bool,
}

impl SymbolTable {
    pub fn new(symbols: HashMap<String, Symbol>) -> Self {
        Self {
            symbols,
            breakpoints: IndexSet::new(),
            active: true,
        }
    }

    pub fn read_from_file(path: &str) -> Result<Self, Error> {
        let file =
            elf::File::open_stream(&mut File::open(path)?).map_err(<Box<dyn StdError>>::from)?;
        let (symtab, symstrtab) = file
            .symbol_table()
            .map_err(<Box<dyn StdError>>::from)?
            .ok_or(Error::Misc("No symbol table in file"))?;
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

    pub fn update_symbols(&mut self, symbols: HashMap<String, Symbol>) {
        self.breakpoints = self
            .breakpoints
            .iter()
            .cloned()
            .filter(|sym| symbols.contains_key(sym))
            .collect::<IndexSet<_>>();
        self.symbols = symbols;
    }

    pub fn breakpoint_set_at(&self, addr: u32) -> bool {
        self.breakpoints
            .iter()
            .any(|sym| self.symbols[sym].value() == addr)
    }

    pub fn address_to_symbol(&self, addr: u32) -> Option<(&String, u32)> {
        self.symbols
            .iter()
            .filter(|(_, sym)| sym.value() <= addr)
            .map(|(sym_name, sym)| (sym_name, addr - sym.value()))
            .min_by_key(|(_, offset)| *offset)
    }
}
