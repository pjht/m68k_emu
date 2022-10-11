use std::fmt::Display;

use nullable_result::NullableResult;
use serde_yaml::Mapping;

use crate::{card::Card, m68k::BusError, register};

#[derive(Debug)]
pub struct Term;

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Terminal card")
    }
}

impl Card for Term {
    fn new(_data: &Mapping) -> Self
    where
        Self: Sized,
    {
        Self
    }

    fn read_byte_io(&mut self, address: u8) -> NullableResult<u8, BusError> {
        match address {
            0xFF => NullableResult::Ok(3),
            _ => NullableResult::Null,
        }
    }

    fn write_byte_io(&mut self, address: u8, data: u8) -> NullableResult<(), BusError> {
        if address == 0 {
            print!("{}", data as char);
        }
        NullableResult::Ok(())
    }
}

register!(Term, "term");
