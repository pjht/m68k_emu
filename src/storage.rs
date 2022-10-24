use std::{fmt::Display, fs::File, io::Read};

use anyhow::anyhow;
use human_repr::HumanCount;
use nullable_result::NullableResult;
use serde_yaml::Mapping;

use crate::{
    card::{u32_get_be_byte, u32_set_be_byte, Card},
    m68k::BusError,
    register,
};

const SECTOR_SIZE: usize = 256;

#[derive(Debug)]
pub struct Storage {
    data: Vec<u8>,
    sector: u32,
    offset: usize,
    file_name: Option<String>,
}

impl Card for Storage {
    fn new(data: &Mapping) -> anyhow::Result<Self> {
        let file_name = data
            .get("image")
            .map(|name| name.as_str().ok_or_else(|| anyhow!("File name not string")))
            .transpose()?
            .map(ToString::to_string);
        let mut data = Vec::new();
        if let Some(file_name) = file_name.as_ref() {
            File::open(file_name)
                .map_err(|e| anyhow!("Could not open disk image file {} ({})", file_name, e))?
                .read_to_end(&mut data)
                .map_err(|e| anyhow!("Failed to read disk image file {} ({})", file_name, e))?;
        };

        Ok(Self {
            data,
            file_name,
            sector: 0,
            offset: 0,
        })
    }

    fn read_byte_io(&mut self, address: u8) -> NullableResult<u8, BusError> {
        match address {
            0x0..=0x3 => NullableResult::Ok(u32_get_be_byte(self.sector, address)),
            0x4 => {
                let byte = self
                    .data
                    .get(self.sector as usize * SECTOR_SIZE + self.offset as usize)
                    .copied()
                    .unwrap_or(0);
                self.offset += 1;
                NullableResult::Ok(byte)
            }
            0xFF => NullableResult::Ok(4),
            _ => NullableResult::Null,
        }
    }

    fn write_byte_io(&mut self, address: u8, data: u8) -> NullableResult<(), BusError> {
        if let 0x0..=0x3 = address {
            self.sector = u32_set_be_byte(self.sector, address, data);
            self.offset = 0;
        }
        NullableResult::Ok(())
    }

    fn cmd(&mut self, cmd: &[&str]) -> anyhow::Result<()> {
        if cmd[0] == "load" && cmd.len() >= 2 {
            let mut file = File::open(cmd[1])
                .map_err(|e| anyhow!("Couldn't open disk image file {} ({})", cmd[1], e))?;
            self.data.clear();
            file.read_to_end(&mut self.data)
                .map_err(|e| anyhow!("Failed to read disk image file {} ({})", cmd[1], e))?;
            self.file_name = Some(cmd[1].into());
            println!("Read disk image file {}", cmd[1]);
        } else if cmd[0] == "reload" {
            if let Some(file_name) = &self.file_name {
                let mut file = File::open(cmd[1])
                    .map_err(|e| anyhow!("Couldn't open disk image file {} ({})", cmd[1], e))?;
                self.data.clear();
                file.read_to_end(&mut self.data)
                    .map_err(|e| anyhow!("Failed to read disk image file {} ({})", cmd[1], e))?;
                println!("Reloaded disk image file {}", file_name);
            } else {
                println!("No disk image file to reload");
            }
        }
        Ok(())
    }
}

impl Display for Storage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Storage card, ")?;
        if let Some(name) = self.file_name.as_ref() {
            f.write_fmt(format_args!(
                "disk image {} ({})",
                name,
                self.data.len().human_count_bytes(),
            ))
        } else {
            f.write_str("no disk image")
        }
    }
}

register!(Storage, "storage");
