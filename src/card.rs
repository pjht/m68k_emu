use crate::m68k::BusError;
use nullable_result::NullableResult;
use serde_yaml::Mapping;
use std::fmt::{Debug, Display};

pub struct Type {
    pub name: &'static str,
    new: fn(data: &Mapping) -> anyhow::Result<Box<dyn Card>>,
}

impl Type {
    pub const fn new<T: Card + 'static>(name: &'static str) -> Self {
        Self {
            name,
            new: T::new_dyn,
        }
    }

    pub fn new_card(&self, data: &Mapping) -> anyhow::Result<Box<dyn Card>> {
        (self.new)(data)
    }
}

inventory::collect!(Type);

pub trait Card: Debug + Display {
    fn new(data: &Mapping) -> anyhow::Result<Self>
    where
        Self: Sized;
    fn new_dyn(data: &Mapping) -> anyhow::Result<Box<dyn Card>>
    where
        Self: Sized + 'static,
    {
        Ok(Box::new(Self::new(data)?))
    }
    fn display(&self) -> String {
        String::new()
    }
    fn read_byte(&mut self, _address: u32) -> NullableResult<u8, BusError> {
        NullableResult::Null
    }

    fn write_byte(&mut self, _address: u32, _data: u8) -> NullableResult<(), BusError> {
        NullableResult::Null
    }

    fn read_byte_io(&mut self, _address: u8) -> NullableResult<u8, BusError> {
        NullableResult::Null
    }

    fn write_byte_io(&mut self, _address: u8, _data: u8) -> NullableResult<(), BusError> {
        NullableResult::Null
    }

    fn read_word(&mut self, address: u32) -> NullableResult<u16, BusError> {
        assert!((address & 0x1) == 0);
        let upper_byte = self.read_byte(address)?;
        let lower_byte = self.read_byte(address + 1)?;
        NullableResult::Ok((u16::from(upper_byte) << 8) | u16::from(lower_byte))
    }

    fn write_word(&mut self, address: u32, data: u16) -> NullableResult<(), BusError> {
        assert!((address & 0x1) == 0);
        self.write_byte(address, (data >> 8) as u8)?;
        self.write_byte(address + 1, data as u8)?;
        NullableResult::Ok(())
    }

    fn read_word_io(&mut self, address: u8) -> NullableResult<u16, BusError> {
        assert!((address & 0x1) == 0);
        let upper_byte = self.read_byte_io(address)?;
        let lower_byte = self.read_byte_io(address + 1)?;
        NullableResult::Ok((u16::from(upper_byte) << 8) | u16::from(lower_byte))
    }

    fn write_word_io(&mut self, address: u8, data: u16) -> NullableResult<(), BusError> {
        assert!((address & 0x1) == 0);
        self.write_byte_io(address, (data >> 8) as u8)?;
        self.write_byte_io(address + 1, data as u8)?;
        NullableResult::Ok(())
    }

    fn cmd(&mut self, _cmd: &[&str]) -> anyhow::Result<()> {
        Ok(())
    }
    fn reset(&mut self) {}
}

#[allow(dead_code)]
pub const fn u64_set_be_byte(val: u64, idx: u8, byte: u8) -> u64 {
    let mut bytes = val.to_be_bytes();
    bytes[idx as usize] = byte;
    u64::from_be_bytes(bytes)
}

#[allow(dead_code)]
pub const fn u64_get_be_byte(val: u64, idx: u8) -> u8 {
    val.to_be_bytes()[idx as usize]
}

#[allow(dead_code)]
pub const fn u32_set_be_byte(val: u32, idx: u8, byte: u8) -> u32 {
    let mut bytes = val.to_be_bytes();
    bytes[idx as usize] = byte;
    u32::from_be_bytes(bytes)
}

#[allow(dead_code)]
pub const fn u32_get_be_byte(val: u32, idx: u8) -> u8 {
    val.to_be_bytes()[idx as usize]
}

#[allow(dead_code)]
pub const fn u16_set_be_byte(val: u16, idx: u8, byte: u8) -> u16 {
    let mut bytes = val.to_be_bytes();
    bytes[idx as usize] = byte;
    u16::from_be_bytes(bytes)
}

#[allow(dead_code)]
pub const fn u16_get_be_byte(val: u16, idx: u8) -> u8 {
    val.to_be_bytes()[idx as usize]
}

#[macro_export]
macro_rules! register {
    ($typ: ty, $name: literal) => {
        paste::paste! {
            inventory::submit!($crate::card::Type::new::<$typ>($name));
        }
    };
}
