use std::{fmt::Display, path::Path};

use crate::{
    location::Location,
    symbol::Symbol,
    symbol_table::{InvalidSymbolName, SymbolTable},
};
use indexmap::IndexMap;
use itertools::Itertools;
use parse_int::parse;
use thiserror::Error;

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

pub struct SymbolDisplayer<'a>(&'a SymbolTables);

impl Display for SymbolDisplayer<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        displayer_common(self.0, f, |table| table.symbol_displayer())
    }
}

#[derive(Debug, Copy, Clone, Error)]
#[error("Invalid symbol table")]
struct InvalidSymbolTable;

pub struct BreakpointDisplayer<'a>(&'a SymbolTables);

impl Display for BreakpointDisplayer<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        displayer_common(self.0, f, |table| table.breakpoint_displayer())
    }
}

pub struct SymbolTables {
    tables: IndexMap<String, SymbolTable>,
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

    pub fn delete(&mut self, table: &str) -> anyhow::Result<SymbolTable> {
        Ok(self.tables.shift_remove(table).ok_or(InvalidSymbolTable)?)
    }

    pub fn set_active(&mut self, table: &str, active: bool) -> anyhow::Result<()> {
        self.get_table_mut(table)?.active = active;
        Ok(())
    }

    pub fn load_table(&mut self, path: &str, append: bool) -> anyhow::Result<()> {
        let new_table = SymbolTable::read_from_file(path)?;
        let table_name = Path::new(&path).file_name().unwrap().to_str().unwrap();
        if let Some(table) = self.tables.get_mut(table_name) {
            table.update_symbols_from(new_table);
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

    pub fn set_breakpoint(&mut self, table: &str, symbol: String) -> anyhow::Result<()> {
        self.get_table_mut(table)?.set_breakpoint(symbol);
        Ok(())
    }

    pub fn delete_breakpoint(&mut self, table: &str, symbol: &str) -> anyhow::Result<bool> {
        Ok(self.get_table_mut(table)?.delete_breakpoint(symbol))
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

    pub fn get(&self, table: &str, symbol: &str) -> anyhow::Result<&Symbol> {
        self.get_table(table)?.get_symbol(symbol)
    }

    pub fn parse_location(&self, location: &str) -> anyhow::Result<Location> {
        parse::<u32>(location).map(Location::Address).or_else(|_| {
            let (mut table_name, symbol_name) = location.split_once(':').unwrap_or(("", location));
            if table_name.is_empty() {
                table_name = self
                    .tables
                    .iter()
                    .find(|(_, table)| table.contains_symbol(symbol_name))
                    .ok_or(InvalidSymbolName)?
                    .0;
            } else if !self.get_table(table_name)?.contains_symbol(symbol_name) {
                return Err(InvalidSymbolName.into());
            }
            Ok(Location::Symbol((
                table_name.to_string(),
                symbol_name.to_string(),
            )))
        })
    }

    pub fn parse_location_address(&self, location: &str) -> anyhow::Result<u32> {
        self.parse_location(location).map(|l| l.addr(self))
    }

    fn get_table(&self, table: &str) -> anyhow::Result<&SymbolTable> {
        Ok(self.tables.get(table).ok_or(InvalidSymbolTable)?)
    }

    fn get_table_mut(&mut self, table: &str) -> anyhow::Result<&mut SymbolTable> {
        Ok(self.tables.get_mut(table).ok_or(InvalidSymbolTable)?)
    }
}
