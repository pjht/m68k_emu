use std::fmt::Display;

use crate::SymbolTables;

#[derive(Debug)]
pub enum Location {
    Symbol((String, String)),
    Address(u32),
}

impl Location {
    pub fn addr(&self, symbol_tables: &SymbolTables) -> u32 {
        match self {
            Self::Symbol((table, sym)) => symbol_tables.get(table, sym).unwrap().value(),
            Self::Address(addr) => *addr,
        }
    }

    pub fn displayer<'a>(&'a self, symbol_tables: &'a SymbolTables) -> Displayer<'a> {
        Displayer {
            location: self,
            symbol_tables,
        }
    }
}

pub struct Displayer<'a> {
    location: &'a Location,
    symbol_tables: &'a SymbolTables,
}

impl<'a> Display for Displayer<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.location {
            Location::Symbol((table, sym)) => f.write_fmt(format_args!(
                "{}:{} ({:#x})",
                table,
                sym,
                self.location.addr(self.symbol_tables)
            )),
            Location::Address(addr) => f.write_fmt(format_args!("{:#x}", addr)),
        }
    }
}
