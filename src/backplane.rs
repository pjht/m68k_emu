use nullable_result::GeneralIterExt;
use serde_yaml::Mapping;
use thiserror::Error;

use crate::{
    card::{self, Card},
    m68k::BusError,
};

#[derive(Debug)]
pub struct Backplane {
    cards: Vec<Box<dyn Card>>,
}

#[derive(Debug, Copy, Clone, Error)]
pub enum CardAddError {
    #[error["Backplane full, could not add card"]]
    BackplaneFull,
    #[error("Invalid card type")]
    InvalidType,
}

impl Backplane {
    pub fn new() -> Self {
        Self { cards: Vec::new() }
    }

    #[allow(dead_code)]
    pub fn cards(&self) -> &[Box<dyn Card>] {
        self.cards.as_ref()
    }

    pub fn cards_mut(&mut self) -> &mut Vec<Box<dyn Card>> {
        &mut self.cards
    }

    pub fn add_card(&mut self, type_name: &str, config: &Mapping) -> anyhow::Result<usize> {
        if self.cards.len() >= 255 {
            return Err(CardAddError::BackplaneFull.into());
        }
        self.cards.push(
            inventory::iter::<card::Type>()
                .find(|card_type| card_type.name == type_name)
                .ok_or(CardAddError::InvalidType)?
                .new_card(config)?,
        );
        Ok(self.cards.len() - 1)
    }

    pub fn read_word(&mut self, address: u32) -> Result<u16, BusError> {
        match address {
            (0..=0x00fe_ffff) | (0x0100_0000..=0xFFFF_FFFF) => self
                .cards
                .iter_mut()
                .try_find_map(|card| card.read_word(address))
                .result(BusError),
            (0x00ff_0000..=0x00ff_00ff) => Ok(0),
            (0x00ff_0100..=0x00ff_ffff) => self
                .cards
                .get_mut(((address >> 8) as u8 - 1) as usize)
                .map_or(Ok(0), |card| {
                    card.read_word_io(address as u8)
                        .optional_result()
                        .unwrap_or(Ok(0))
                }),
        }
    }

    pub fn read_byte(&mut self, address: u32) -> Result<u8, BusError> {
        match address {
            (0..=0x00fe_ffff) | (0x0100_0000..=0xFFFF_FFFF) => self
                .cards
                .iter_mut()
                .try_find_map(|card| card.read_byte(address))
                .result(BusError),
            (0x00ff_0000..=0x00ff_00ff) => Ok(0),
            (0x00ff_0100..=0x00ff_ffff) => self
                .cards
                .get_mut(((address >> 8) as u8 - 1) as usize)
                .map_or(Ok(0), |card| {
                    card.read_byte_io(address as u8)
                        .optional_result()
                        .unwrap_or(Ok(0))
                }),
        }
    }

    pub fn write_word(&mut self, address: u32, data: u16) -> Result<(), BusError> {
        match address {
            (0..=0x00fe_ffff) | (0x0100_0000..=0xFFFF_FFFF) => self
                .cards
                .iter_mut()
                .try_find_map(|card| card.write_word(address, data))
                .result(BusError),
            (0x00ff_0000..=0x00ff_00ff) => Ok(()),
            (0x00ff_0100..=0x00ff_ffff) => self
                .cards
                .get_mut(((address >> 8) as u8 - 1) as usize)
                .map_or(Ok(()), |card| {
                    card.write_word_io(address as u8, data)
                        .optional_result()
                        .unwrap_or(Ok(()))
                }),
        }
    }

    pub fn write_byte(&mut self, address: u32, data: u8) -> Result<(), BusError> {
        match address {
            (0..=0x00fe_ffff) | (0x0100_0000..=0xFFFF_FFFF) => self
                .cards
                .iter_mut()
                .try_find_map(|card| card.write_byte(address, data))
                .result(BusError),
            (0x00ff_0000..=0x00ff_00ff) => Ok(()),
            (0x00ff_0100..=0x00ff_ffff) => self
                .cards
                .get_mut(((address >> 8) as u8 - 1) as usize)
                .map_or(Ok(()), |card| {
                    card.write_byte_io(address as u8, data)
                        .optional_result()
                        .unwrap_or(Ok(()))
                }),
        }
    }
}
