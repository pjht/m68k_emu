use std::fmt::Display;

use human_repr::HumanCount;
use nullable_result::NullableResult;
use serde_yaml::Mapping;

use crate::{
    card::{u32_get_be_byte, Card},
    m68k::BusError,
    register,
};

#[derive(Debug)]
pub struct Ram {
    data: Vec<u8>,
    start: u32,
    enabled: bool,
}

impl Ram {}

impl Card for Ram {
    fn new(data: &Mapping) -> Self {
        let size = data
            .get("size")
            .expect("No size value for RAM")
            .as_u64()
            .expect("Size value not positive integer");
        Self {
            data: vec![0; size as usize],
            start: 0,
            enabled: false,
        }
    }
    fn read_byte(&mut self, address: u32) -> NullableResult<u8, BusError> {
        if !self.enabled {
            return NullableResult::Null;
        }
        let address = address.checked_sub(self.start)?;
        self.data.get(address as usize).copied().into()
    }

    fn write_byte(&mut self, address: u32, data: u8) -> NullableResult<(), BusError> {
        if !self.enabled {
            return NullableResult::Null;
        }
        let address = address.checked_sub(self.start)?;
        if address >= self.data.len() as u32 {
            return NullableResult::Null;
        }
        self.data[address as usize] = data;
        NullableResult::Ok(())
    }

    fn write_byte_io(&mut self, address: u8, data: u8) -> NullableResult<(), BusError> {
        match address {
            0 => self.start = self.start & !0xFF00_0000 | (u32::from(data) << 24),
            1 => self.start = self.start & !0x00FF_0000 | (u32::from(data) << 16),
            2 => self.start = self.start & !0x0000_FF00 | (u32::from(data) << 8),
            3 => {
                self.start = self.start & !0x0000_00FF | u32::from(data & 0xFE);
                self.enabled = data & 0x1 > 0;
            }
            _ => (),
        }
        NullableResult::Ok(())
    }

    fn read_byte_io(&mut self, address: u8) -> NullableResult<u8, BusError> {
        match address {
            0 => NullableResult::Ok((self.start >> 24) as u8),
            1 => NullableResult::Ok((self.start >> 16) as u8),
            2 => NullableResult::Ok((self.start >> 8) as u8),
            3 => NullableResult::Ok((self.start as u8) | u8::from(self.enabled)),
            (4..=7) => NullableResult::Ok(u32_get_be_byte(self.data.len() as u32, address - 4)),
            0xFF => NullableResult::Ok(2),
            _ => NullableResult::Null,
        }
    }

    fn reset(&mut self) {
        self.enabled = false;
    }
}

impl Display for Ram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "RAM card, size {}",
            self.data.len().human_count_bytes()
        ))?;
        if self.enabled {
            f.write_fmt(format_args!(", enabled at base address {:#x}", self.start))?;
        };
        Ok(())
    }
}

register!(Ram, "ram");
