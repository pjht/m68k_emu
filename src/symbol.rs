use std::fmt::Display;

use elf::symbol::Symbol as ElfSymbol;

#[derive(Debug)]
pub struct Symbol {
    section: u16,
    value: u32,
    size: u32,
}

impl Symbol {
    #[allow(unused)]
    pub fn section(&self) -> u16 {
        self.section
    }

    pub fn value(&self) -> u32 {
        self.value
    }

    #[allow(unused)]
    pub fn size(&self) -> u32 {
        self.size
    }
}

impl From<ElfSymbol> for Symbol {
    fn from(sym: ElfSymbol) -> Self {
        Self {
            section: sym.st_shndx,
            value: sym.st_value as u32,
            size: sym.st_size as u32,
        }
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "Value: {:#010x} Size: {:#06x} Section: {}",
            self.value, self.size, self.section
        ))
    }
}
