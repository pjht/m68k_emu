use crate::symbol::Symbol;
use indexmap::IndexSet;
use std::collections::HashMap;

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

    pub fn update_symbols(&mut self, symbols: HashMap<String, Symbol>) {
        self.breakpoints = self
            .breakpoints
            .iter()
            .cloned()
            .filter(|sym| symbols.contains_key(sym))
            .collect::<IndexSet<_>>();
        self.symbols = symbols;
    }
}
