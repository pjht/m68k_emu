use std::{fmt::Display, fs::File, io::Read};

use anyhow::anyhow;
use human_repr::HumanCount;
use nullable_result::NullableResult;
use serde_yaml::Mapping;

use crate::{
    card::{u16_get_be_byte, u16_set_be_byte, Card},
    m68k::BusError,
    register,
};

#[derive(Debug)]
pub struct Rom {
    data: Vec<u8>,
    enabled: bool,
    ram: [u8; 32 * 1024],
    file_name: Option<String>,
    start: u16,
}

impl Rom {}

impl Card for Rom {
    fn new(data: &Mapping) -> anyhow::Result<Self> {
        let file_name = data
            .get("image")
            .map(|name| name.as_str().ok_or_else(|| anyhow!("File name not string")))
            .transpose()?
            .map(|name| name.to_string());
        let mut data = Vec::new();
        if let Some(file_name) = file_name.as_ref() {
            File::open(file_name)
                .map_err(|e| anyhow!("Could not open ROM image file {} ({})", file_name, e))?
                .read_to_end(&mut data)
                .map_err(|e| anyhow!("Failed to read ROM image file {} ({})", file_name, e))?;
        };
        Ok(Self {
            data,
            enabled: true,
            ram: [0; 32 * 1024],
            file_name,
            start: 0,
        })
    }

    fn read_byte(&mut self, address: u32) -> NullableResult<u8, BusError> {
        if !self.enabled | ((address >> 16) as u16 != self.start) {
            return NullableResult::Null;
        }
        let address = address as u16;
        if address < 0x4000 {
            self.data.get(address as usize).copied().into()
        } else {
            self.ram.get((address - 0x4000) as usize).copied().into()
        }
    }

    fn write_byte(&mut self, address: u32, data: u8) -> NullableResult<(), BusError> {
        if !self.enabled | ((address >> 16) as u16 != self.start) {
            return NullableResult::Null;
        }
        let address = (address as u16).checked_sub(0x4000)?;
        if address > self.data.len() as u16 {
            return NullableResult::Null;
        }
        self.ram[address as usize] = data;
        NullableResult::Ok(())
    }

    fn read_byte_io(&mut self, address: u8) -> NullableResult<u8, BusError> {
        match address {
            (0..=0xEF) => NullableResult::Ok(self.ram[address as usize]),
            (0xF0..=0xF1) => NullableResult::Ok(u16_get_be_byte(self.start, address - 0xF0)),
            0xFE => NullableResult::Ok(self.enabled as u8),
            0xFF => NullableResult::Ok(1),
            _ => NullableResult::Null,
        }
    }

    fn write_byte_io(&mut self, address: u8, data: u8) -> NullableResult<(), BusError> {
        match address {
            (0..=0xEF) => {
                self.ram[address as usize] = data;
            }
            (0xF0..=0xF1) => {
                self.start = u16_set_be_byte(self.start, address - 0xF0, data);
            }
            0xFE => {
                self.enabled = data > 0;
            }
            _ => (),
        }
        NullableResult::Ok(())
    }

    fn cmd(&mut self, cmd: &[&str]) -> anyhow::Result<()> {
        if cmd[0] == "load" && cmd.len() >= 2 {
            let mut file = File::open(cmd[1])
                .map_err(|e| anyhow!("Couldn't open ROM image file {} ({})", cmd[1], e))?;
            self.data.clear();
            file.read_to_end(&mut self.data)
                .map_err(|e| anyhow!("Failed to read ROM image file {} ({})", cmd[1], e))?;
            self.file_name = Some(cmd[1].into());
            println!("Read ROM image file {}", cmd[1]);
        } else if cmd[0] == "reload" {
            if let Some(file_name) = &self.file_name {
                let mut file = File::open(cmd[1])
                    .map_err(|e| anyhow!("Couldn't open ROM image file {} ({})", cmd[1], e))?;
                self.data.clear();
                file.read_to_end(&mut self.data)
                    .map_err(|e| anyhow!("Failed to read ROM image file {} ({})", cmd[1], e))?;
                println!("Reloaded ROM image file {}", file_name);
            } else {
                println!("No ROM image file to reload");
            }
        }
        Ok(())
    }

    fn reset(&mut self) {
        self.enabled = true;
    }
}

impl Display for Rom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("ROM card, ")?;
        if let Some(name) = self.file_name.as_ref() {
            f.write_fmt(format_args!(
                "image {} ({})",
                name,
                self.data.len().human_count_bytes()
            ))?;
        } else {
            f.write_str("no image")?;
        };
        if self.enabled {
            f.write_fmt(format_args!(
                ", enabled at base address {:#x}",
                (self.start as u32) << 16
            ))?;
        };
        Ok(())
    }
}

register!(Rom, "rom");
