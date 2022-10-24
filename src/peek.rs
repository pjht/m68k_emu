use thiserror::Error;

#[derive(Copy, Clone, Debug)]
pub enum Format {
    Octal,
    Hex,
    Decimal,
    UnsignedDecimal,
    Binary,
}

impl Format {
    pub fn format(self, num: u32, size: Size) -> String {
        match self {
            Self::Octal => format!("Oo{:0>width$o}", num, width = size.byte_count()),
            Self::Hex => format!("0x{:0>width$x}", num, width = size.byte_count() * 2),
            Self::Decimal => {
                let num = match size {
                    Size::Byte => num as u8 as i8 as i32,
                    Size::Word => num as u16 as i16 as i32,
                    Size::LongWord => num as i32,
                };
                format!("{}", num)
            }
            Self::UnsignedDecimal => format!("{}", num),
            Self::Binary => format!("0b{:0>width$b}", num, width = size.byte_count() * 8),
        }
    }
}

#[derive(Debug, Copy, Clone, Error)]
#[error("Invalid peek format")]
pub struct InvalidFormat;

impl TryFrom<char> for Format {
    type Error = InvalidFormat;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'o' => Ok(Self::Octal),
            'x' => Ok(Self::Hex),
            'd' => Ok(Self::Decimal),
            'u' => Ok(Self::UnsignedDecimal),
            'b' => Ok(Self::Binary),
            _ => Err(InvalidFormat),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Size {
    Byte,
    Word,
    LongWord,
}

impl Size {
    pub fn byte_count(self) -> usize {
        match self {
            Self::Byte => 1,
            Self::Word => 2,
            Self::LongWord => 4,
        }
    }

    pub fn chunk_size(self) -> usize {
        match self {
            Self::Byte | Self::Word => 8,
            Self::LongWord => 4,
        }
    }
}

#[derive(Debug, Copy, Clone, Error)]
#[error("Invalid peek size")]
pub struct InvalidSize;

impl TryFrom<char> for Size {
    type Error = InvalidSize;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'b' => Ok(Self::Byte),
            'w' => Ok(Self::Word),
            'l' => Ok(Self::LongWord),
            _ => Err(InvalidSize),
        }
    }
}
