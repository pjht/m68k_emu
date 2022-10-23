use thiserror::Error;

#[derive(Copy, Clone, Debug)]
pub enum PeekFormat {
    Octal,
    Hex,
    Decimal,
    UnsignedDecimal,
    Binary,
}

impl PeekFormat {
    pub fn format(self, num: u32, size: PeekSize) -> String {
        match self {
            Self::Octal => format!("Oo{:0>width$o}", num, width = size.byte_count()),
            Self::Hex => format!("0x{:0>width$x}", num, width = size.byte_count() * 2),
            Self::Decimal => {
                let num = match size {
                    PeekSize::Byte => num as u8 as i8 as i32,
                    PeekSize::Word => num as u16 as i16 as i32,
                    PeekSize::LongWord => num as i32,
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
pub struct InvalidPeekFormat;

impl TryFrom<char> for PeekFormat {
    type Error = InvalidPeekFormat;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'o' => Ok(Self::Octal),
            'x' => Ok(Self::Hex),
            'd' => Ok(Self::Decimal),
            'u' => Ok(Self::UnsignedDecimal),
            'b' => Ok(Self::Binary),
            _ => Err(InvalidPeekFormat),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum PeekSize {
    Byte,
    Word,
    LongWord,
}

impl PeekSize {
    pub fn byte_count(self) -> usize {
        match self {
            PeekSize::Byte => 1,
            PeekSize::Word => 2,
            PeekSize::LongWord => 4,
        }
    }

    pub fn chunk_size(self) -> usize {
        match self {
            PeekSize::Byte => 8,
            PeekSize::Word => 8,
            PeekSize::LongWord => 4,
        }
    }
}

#[derive(Debug, Copy, Clone, Error)]
#[error("Invalid peek size")]
pub struct InvalidPeekSize;

impl TryFrom<char> for PeekSize {
    type Error = InvalidPeekSize;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'b' => Ok(Self::Byte),
            'w' => Ok(Self::Word),
            'l' => Ok(Self::LongWord),
            _ => Err(InvalidPeekSize),
        }
    }
}
