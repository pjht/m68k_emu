use std::{fmt::Display, path::Path};

use crate::{error::Error, location::Location, symbol::Symbol, symbol_table::SymbolTable};
use indexmap::IndexMap;
use itertools::Itertools;
use parse_int::parse;

pub struct SymbolDisplayer<'a>(&'a SymbolTables);

fn displayer_common<'a, F, T: Display>(
    symbol_tables: &'a SymbolTables,
    f: &mut std::fmt::Formatter<'_>,
    mut table_fmt: F,
) -> std::fmt::Result
where
    F: FnMut(&'a SymbolTable) -> T,
{
    f.write_fmt(format_args!(
        "{}",
        symbol_tables
            .tables
            .iter()
            .format_with("\n", |(table_name, table), f| {
                f(&format_args!(
                    "{table_name} ({}):\n{}",
                    if table.active { "active" } else { "inactive" },
                    table_fmt(table),
                ))
            })
    ))
}

impl Display for SymbolDisplayer<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        displayer_common(self.0, f, |table| {
            table.symbols.iter().format_with("\n", |(name, symbol), g| {
                g(&format_args!("{name}: {symbol}"))
            })
        })
    }
}

pub struct BreakpointDisplayer<'a>(&'a SymbolTables);

impl Display for BreakpointDisplayer<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        displayer_common(self.0, f, |table| table.breakpoints.iter().format("\n"))
    }
}

pub struct SymbolTables {
    pub tables: IndexMap<String, SymbolTable>,
}

impl SymbolTables {
    pub fn new() -> Self {
        Self {
            tables: IndexMap::new(),
        }
    }
    pub fn breakpoint_set_at(&self, addr: u32) -> bool {
        self.tables
            .values()
            .any(|table| table.active && table.breakpoint_set_at(addr))
    }

    pub fn address_to_symbol(&self, addr: u32) -> Option<(&String, &String, u32)> {
        self.tables
            .iter()
            .filter(|(_, table)| table.active)
            .find_map(|(table_name, table)| {
                table
                    .address_to_symbol(addr)
                    .map(|(sym_name, offset)| (table_name, sym_name, offset))
            })
    }

    pub fn delete(&mut self, table: &str) -> Result<SymbolTable, Error> {
        self.tables
            .shift_remove(table)
            .ok_or(Error::InvalidSymbolTable)
    }

    pub fn set_active(&mut self, table: &str, active: bool) -> Result<(), Error> {
        self.tables
            .get_mut(table)
            .ok_or(Error::InvalidSymbolTable)?
            .active = active;
        Ok(())
    }

    pub fn load_table(&mut self, path: &str, append: bool) -> Result<(), Error> {
        let new_table = SymbolTable::read_from_file(path)?;
        let table_name = Path::new(&path).file_name().unwrap().to_str().unwrap();
        if let Some(table) = self.tables.get_mut(table_name) {
            table.update_symbols(new_table.symbols);
        } else {
            self.tables.insert(table_name.to_string(), new_table);
        };
        if !append {
            let table = self.tables.remove(table_name).unwrap();
            self.tables.clear();
            self.tables.insert(table_name.to_string(), table);
        };
        Ok(())
    }

    pub fn set_breakpoint(&mut self, table: &str, symbol: String) -> Result<(), Error> {
        self.tables
            .get_mut(table)
            .ok_or(Error::InvalidSymbolTable)?
            .breakpoints
            .insert(symbol);
        Ok(())
    }

    pub fn delete_breakpoint(&mut self, table: &str, symbol: &str) -> Result<bool, Error> {
        Ok(self
            .tables
            .get_mut(table)
            .ok_or(Error::InvalidSymbolTable)?
            .breakpoints
            .shift_remove(symbol))
    }

    pub fn symbol_displayer(&self) -> SymbolDisplayer<'_> {
        SymbolDisplayer(self)
    }

    pub fn breakpoint_displayer(&self) -> BreakpointDisplayer {
        BreakpointDisplayer(self)
    }

    pub fn is_empty(&self) -> bool {
        self.tables.is_empty()
    }

    pub fn get(&self, table: &str, symbol: &str) -> Result<&Symbol, Error> {
        self.tables
            .get(table)
            .ok_or(Error::InvalidSymbolTable)?
            .symbols
            .get(symbol)
            .ok_or(Error::InvalidSymbolName)
    }

    pub fn parse_location(&self, location: &str) -> Result<Location, Error> {
        parse::<u32>(location).map(Location::Address).or_else(|_| {
            let (mut table_name, symbol_name) = location.split_once(':').unwrap_or(("", location));
            if table_name.is_empty() {
                table_name = self
                    .tables
                    .iter()
                    .find(|(_, table)| table.symbols.contains_key(symbol_name))
                    .ok_or(Error::InvalidSymbolName)?
                    .0;
            } else if !self
                .tables
                .get(table_name)
                .ok_or(Error::InvalidSymbolTable)?
                .symbols
                .contains_key(symbol_name)
            {
                return Err(Error::InvalidSymbolName);
            }
            Ok(Location::Symbol((
                table_name.to_string(),
                symbol_name.to_string(),
            )))
        })
    }

    pub fn parse_location_address(&self, location: &str) -> Result<u32, Error> {
        self.parse_location(location).map(|l| l.addr(self))
    }
}
