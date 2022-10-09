use std::fmt::Display;

use derive_try_from_primitive::TryFromPrimitive;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RegisterEffective {
    DataReg(u8),
    AddressReg(u8),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FromEffectiveError;

impl TryFrom<EffectiveAddress> for RegisterEffective {
    type Error = FromEffectiveError;

    fn try_from(value: EffectiveAddress) -> Result<Self, Self::Error> {
        match value {
            EffectiveAddress::DataReg(x) => Ok(Self::DataReg(x)),
            EffectiveAddress::AddressReg(x) => Ok(Self::AddressReg(x)),
            _ => Err(FromEffectiveError),
        }
    }
}

impl Display for RegisterEffective {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&EffectiveAddress::from(*self), f)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum EffectiveAddress {
    DataReg(u8),
    AddressReg(u8),
    Address(u8),
    AddressPostinc(u8),
    AddressPredec(u8),
    AddressDisplacement(u8, i16),
    AddressIndex {
        reg: u8,
        displacement: u8,
        idx: RegisterEffective,
        idx_size: Size,
    },
    PcDisplacement(u32, u16),
    PcIndex(u32, u8, RegisterEffective, Size),
    AbsoluteShort(u16),
    AbsoluteLong(u32),
    Immediate(u32),
}

impl From<RegisterEffective> for EffectiveAddress {
    fn from(value: RegisterEffective) -> Self {
        match value {
            RegisterEffective::DataReg(x) => Self::DataReg(x),
            RegisterEffective::AddressReg(x) => Self::AddressReg(x),
        }
    }
}

impl Display for EffectiveAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DataReg(r) => f.write_fmt(format_args!("D{}", r)),
            Self::AddressReg(r) => f.write_fmt(format_args!("A{}", r)),
            Self::Address(r) => f.write_fmt(format_args!("(A{})", r)),
            Self::AddressPostinc(r) => f.write_fmt(format_args!("(A{})+", r)),
            Self::AddressPredec(r) => f.write_fmt(format_args!("-(A{})", r)),
            Self::AddressDisplacement(r, d) => f.write_fmt(format_args!("({}, A{})", d, r)),
            Self::AddressIndex {
                reg,
                displacement,
                idx,
                ..
            } => f.write_fmt(format_args!("({}, A{}, {})", displacement, reg, idx)),
            Self::PcDisplacement(_, d) => f.write_fmt(format_args!("(0x{:x}, PC)", d)),
            Self::PcIndex(_, d, idx, _) => f.write_fmt(format_args!("({}, PC, {})", d, idx)),
            Self::AbsoluteShort(a) => f.write_fmt(format_args!("(0x{:x}).W", a)),
            Self::AbsoluteLong(a) => f.write_fmt(format_args!("(0x{:x}).L", a)),
            Self::Immediate(i) => f.write_fmt(format_args!("#0x{:x}", i)),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Size {
    Byte,
    Word,
    Long,
}

impl Size {
    pub fn byte_size(self) -> u8 {
        match self {
            Self::Byte => 1,
            Self::Word => 2,
            Self::Long => 4,
        }
    }
}

impl Display for Size {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Byte => f.write_str("B"),
            Self::Word => f.write_str("W"),
            Self::Long => f.write_str("L"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ArithType {
    Reg,
    Addr,
    Ext,
}

#[derive(TryFromPrimitive, Debug, Copy, Clone, PartialEq, Eq)]
#[repr(u8)]
pub enum Condition {
    True,
    False,
    Higher,
    LowerOrSame,
    CarryClear,
    CarrySet,
    NotEqual,
    Equal,
    OverflowClear,
    OverflowSet,
    Plus,
    Minus,
    GreaterOrEqual,
    LessThan,
    GreaterThan,
    LessOrEqual,
}

#[allow(clippy::nonminimal_bool)] // Exact formula is preserved
impl Condition {
    pub fn matches(self, cc: u8) -> bool {
        let carry = (cc & 0x1) > 0;
        let overflow = (cc & 0x2) > 0;
        let zero = (cc & 0x4) > 0;
        let negative = (cc & 0x8) > 0;
        match self {
            Self::True => true,
            Self::False => false,
            Self::Higher => !carry && !zero,
            Self::LowerOrSame => carry | zero,
            Self::CarryClear => !carry,
            Self::CarrySet => carry,
            Self::NotEqual => !zero,
            Self::Equal => zero,
            Self::OverflowClear => !overflow,
            Self::OverflowSet => overflow,
            Self::Plus => !negative,
            Self::Minus => negative,
            Self::GreaterOrEqual => (negative && overflow) || (!negative && !overflow),
            Self::LessThan => (negative && !overflow) || (!negative && overflow),
            Self::GreaterThan => {
                (negative && overflow && !zero) || (!negative && !overflow && !zero)
            }
            Self::LessOrEqual => zero || (negative && !overflow) || (!negative && overflow),
        }
    }
}

impl Display for Condition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::True => "T",
            Self::False => "F",
            Self::Higher => "HI",
            Self::LowerOrSame => "LS",
            Self::CarryClear => "CC",
            Self::CarrySet => "CS",
            Self::NotEqual => "NE",
            Self::Equal => "EQ",
            Self::OverflowClear => "VC",
            Self::OverflowSet => "VS",
            Self::Plus => "PL",
            Self::Minus => "MI",
            Self::GreaterOrEqual => "GE",
            Self::LessThan => "LT",
            Self::GreaterThan => "GT",
            Self::LessOrEqual => "LE",
        })
    }
}

#[derive(Debug, Clone)]
pub enum BitInsType {
    Test,
    Change,
    Clear,
    Set,
}

impl Display for BitInsType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Test => write!(f, "BTST"),
            Self::Change => write!(f, "BCHG"),
            Self::Clear => write!(f, "BCLR"),
            Self::Set => write!(f, "BSET"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum MoveDirection {
    MemToReg,
    RegToMem,
}

#[derive(Debug, Clone)]
pub enum Rotation {
    Immediate(u8),
    Register(u8),
}

impl Display for Rotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Immediate(val) => write!(f, "#{val}"),
            Self::Register(reg) => write!(f, "D{reg}"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ShiftType {
    Arithmetic,
    Logical,
    RotateExtend,
    Rotate,
}

impl Display for ShiftType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Arithmetic => write!(f, "AS"),
            Self::Logical => write!(f, "LS"),
            Self::RotateExtend => write!(f, "ROX"),
            Self::Rotate => write!(f, "RO"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ShiftDirection {
    Left,
    Right,
}

impl Display for ShiftDirection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Left => write!(f, "L"),
            Self::Right => write!(f, "R"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    OriCcr(u8),
    OriSr(u16),
    Ori(Size, EffectiveAddress, u32),
    AndiCcr(u8),
    AndiSr(u16),
    Andi(Size, EffectiveAddress, u32),
    Subi(Size, EffectiveAddress, u32),
    Addi(Size, EffectiveAddress, u32),
    EoriCcr(u8),
    EoriSr(u16),
    Eori(Size, EffectiveAddress, u32),
    Cmpi(Size, EffectiveAddress, u32),
    Bit {
        typ: BitInsType,
        idx: EffectiveAddress,
        dst: EffectiveAddress,
        size: Size,
    },
    Movep {
        dir: MoveDirection,
        size: Size,
        dreg: u8,
        areg: u8,
        start_offset: i16,
    },
    Move {
        src: EffectiveAddress,
        dst: EffectiveAddress,
        size: Size,
    },
    MoveFromSr(EffectiveAddress),
    MoveToCcr(EffectiveAddress),
    MoveToSr(EffectiveAddress),
    Negx(Size, EffectiveAddress),
    Clr(Size, EffectiveAddress),
    Neg(Size, EffectiveAddress),
    Not(Size, EffectiveAddress),
    Ext(Size, u8),
    Nbcd(EffectiveAddress),
    Swap(u8),
    Pea(EffectiveAddress),
    Tas(EffectiveAddress),
    Tst(Size, EffectiveAddress),
    Trap(u8),
    Link {
        areg: u8,
        displacement: u16,
    },
    Unlk(u8),
    MoveUsp(MoveDirection, u8),
    Reset,
    Nop,
    Stop(u16),
    Rte,
    Rts,
    Trapv,
    Rtr,
    Jsr(EffectiveAddress),
    Jmp(EffectiveAddress),
    Movem(MoveDirection, Size, EffectiveAddress, Vec<EffectiveAddress>),
    Lea(u8, EffectiveAddress),
    Chk(u8, EffectiveAddress),
    Addq(Size, i8, EffectiveAddress),
    Subq(Size, i8, EffectiveAddress),
    Scc(Condition, EffectiveAddress),
    Dbcc(Condition, u8, i16, u32),
    Bra(Size, i16, u32),
    Bsr(Size, i16, u32),
    Bcc(Size, Condition, i16, u32),
    Moveq {
        dreg: u8,
        imm: i8,
    },
    Divu(u8, EffectiveAddress),
    Divs(u8, EffectiveAddress),
    Sbcd {
        src: EffectiveAddress,
        dst: EffectiveAddress,
    },
    Or {
        size: Size,
        src: EffectiveAddress,
        dst: EffectiveAddress,
    },
    Sub {
        size: Size,
        src: EffectiveAddress,
        dst: EffectiveAddress,
    },
    Subx {
        src: EffectiveAddress,
        dst: EffectiveAddress,
        size: Size,
    },
    Suba(u8, Size, EffectiveAddress),
    Eor {
        size: Size,
        src: EffectiveAddress,
        dst: EffectiveAddress,
    },
    Cmpm {
        size: Size,
        src: u8,
        dst: u8,
    },
    Cmp(u8, Size, EffectiveAddress),
    Cmpa(u8, Size, EffectiveAddress),
    Mulu(u8, EffectiveAddress),
    Muls(u8, EffectiveAddress),
    Abcd {
        src: EffectiveAddress,
        dst: EffectiveAddress,
    },
    Exg {
        src: EffectiveAddress,
        dst: EffectiveAddress,
    },
    And {
        size: Size,
        src: EffectiveAddress,
        dst: EffectiveAddress,
    },
    Add {
        size: Size,
        src: EffectiveAddress,
        dst: EffectiveAddress,
    },
    Addx {
        src: EffectiveAddress,
        dst: EffectiveAddress,
        size: Size,
    },
    Adda(u8, Size, EffectiveAddress),
    Shift(ShiftType, Size, ShiftDirection, Rotation, EffectiveAddress),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::OriCcr(val) => write!(f, "ORI #{val:#x}, CCR"),
            Self::OriSr(val) => write!(f, "ORI #{val:#x}, SR"),
            Self::Ori(size, dst, val) => write!(f, "ORI.{size} #{val:#x}, {dst}"),
            Self::AndiCcr(val) => write!(f, "ANDI #{val:#x}, CCR"),
            Self::AndiSr(val) => write!(f, "ANDI #{val:#x}, SR"),
            Self::Andi(size, dst, val) => write!(f, "ANDI.{size} #{val:#x}, {dst}"),
            Self::Subi(size, dst, val) => write!(f, "SUBI.{size} #{val:#x}, {dst}"),
            Self::Addi(size, dst, val) => write!(f, "ADDI.{size} #{val:#x}, {dst}"),
            Self::EoriCcr(val) => write!(f, "EORI #{val:#x}, CCR"),
            Self::EoriSr(val) => write!(f, "EORI #{val:#x}, SR"),
            Self::Eori(size, dst, val) => write!(f, "EORI.{size} #{val:#x}, {dst}"),
            Self::Cmpi(size, dst, val) => write!(f, "CMPI.{size} #{val:#x}, {dst}"),
            Self::Bit {
                typ,
                idx,
                dst,
                size: _,
            } => write!(f, "{typ} {idx}, {dst}"),
            Self::Movep {
                dir,
                size,
                dreg,
                areg,
                start_offset,
            } => match dir {
                MoveDirection::MemToReg => {
                    write!(f, "MOVEP.{size} ({start_offset:#x}, A{areg}), D{dreg}")
                }
                MoveDirection::RegToMem => {
                    write!(f, "MOVEP.{size} D{dreg}, ({start_offset:#x}, A{areg})")
                }
            },
            Self::Move { src, dst, size } => write!(f, "MOVE.{size} {src}, {dst}"),
            Self::MoveFromSr(dst) => write!(f, "MOVE SR, {dst}"),
            Self::MoveToCcr(src) => write!(f, "MOVE {src}, CCR"),
            Self::MoveToSr(src) => write!(f, "MOVE {src}, SR"),
            Self::Negx(size, dst) => write!(f, "NEGX.{size} {dst}"),
            Self::Clr(size, dst) => write!(f, "CLR.{size} {dst}"),
            Self::Neg(size, dst) => write!(f, "NEG.{size} {dst}"),
            Self::Not(size, dst) => write!(f, "NOT.{size} {dst}"),
            Self::Ext(size, dreg) => write!(f, "EXT.{size} D{dreg}"),
            Self::Nbcd(dst) => write!(f, "NBCD {dst}"),
            Self::Swap(dreg) => write!(f, "SWAP D{dreg}"),
            Self::Pea(ea) => write!(f, "PEA {ea}"),
            Self::Tas(dst) => write!(f, "TAS {dst}"),
            Self::Tst(size, src) => write!(f, "TST.{size} {src}"),
            Self::Trap(vec) => write!(f, "TRAP #{vec:#x}"),
            Self::Link { areg, displacement } => write!(f, "LINK A{areg}, #{displacement:#x}"),
            Self::Unlk(areg) => write!(f, "UNLK A{areg}"),
            Self::MoveUsp(dir, areg) => match dir {
                MoveDirection::MemToReg => write!(f, "MOVE USP, A{areg}"),
                MoveDirection::RegToMem => write!(f, "MOVE A{areg}, USP"),
            },
            Self::Reset => write!(f, "RESET"),
            Self::Nop => write!(f, "NOP"),
            Self::Stop(sr) => write!(f, "STOP #{sr:#x}"),
            Self::Rte => write!(f, "RTE"),
            Self::Rts => write!(f, "RTS"),
            Self::Trapv => write!(f, "TRAPV"),
            Self::Rtr => write!(f, "RTR"),
            Self::Jsr(ea) => write!(f, "JSR {ea}"),
            Self::Jmp(ea) => write!(f, "JMP {ea}"),
            Self::Movem(dir, size, dst, regs) => {
                let regs = regs
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join("/");
                match dir {
                    MoveDirection::MemToReg => write!(f, "MOVEM.{size} {regs}, A{dst}"),
                    MoveDirection::RegToMem => write!(f, "MOVEM.{size} {dst}, {regs}"),
                }
            }
            Self::Lea(areg, ea) => write!(f, "LEA {ea}, A{areg}"),
            Self::Chk(src, bound) => write!(f, "CHK {bound}, D{src}"),
            Self::Addq(size, val, dst) => write!(f, "ADDQ.{size} {val:#x}, {dst}"),
            Self::Subq(size, val, dst) => write!(f, "SUBQ.{size} {val:#x}, {dst}"),
            Self::Scc(cond, dst) => write!(f, "S{cond} {dst}"),
            Self::Dbcc(cond, dreg, disp, new_pc) => {
                write!(f, "DB{cond} {dreg}, {disp:#x} ({new_pc:#x})")
            }
            Self::Bra(size, disp, new_pc) => write!(f, "BRA.{size} {disp:#x} ({new_pc:#x})"),
            Self::Bsr(size, disp, new_pc) => write!(f, "BSR.{size} {disp:#x} ({new_pc:#x})"),
            Self::Bcc(size, cond, disp, new_pc) => {
                write!(f, "B{cond}.{size} {disp:#x} ({new_pc:#x})")
            }
            Self::Moveq { dreg, imm } => write!(f, "MOVEQ #{imm:#x}, D{dreg}"),
            Self::Divu(dst_dreg, src) => write!(f, "DIVU,W {src}, D{dst_dreg}"),
            Self::Divs(dst_dreg, src) => write!(f, "DIVS.W {src}, D{dst_dreg}"),
            Self::Sbcd { src, dst } => write!(f, "SBCD {src}, {dst}"),
            Self::Or { size, src, dst } => write!(f, "OR.{size} {src}, {dst}"),
            Self::Sub { size, src, dst } => write!(f, "SUB.{size} {src}, {dst}"),
            Self::Subx { src, dst, size } => write!(f, "SUBX.{size} {src}, {dst}"),
            Self::Suba(dst_areg, size, src) => write!(f, "SUBA.{size} {src}, A{dst_areg}"),
            Self::Eor { size, src, dst } => write!(f, "EOR.{size} {src}, {dst}"),
            Self::Cmpm { size, src, dst } => write!(f, "CMPM.{size} {src}, {dst}"),
            Self::Cmp(dst_dreg, size, src) => write!(f, "CMP.{size} {src}, D{dst_dreg}"),
            Self::Cmpa(dst_areg, size, src) => write!(f, "CMPA.{size} {src}, A{dst_areg}"),
            Self::Mulu(dst_dreg, src) => write!(f, "MULU.W {src}, D{dst_dreg}"),
            Self::Muls(dst_dreg, src) => write!(f, "MULS.W {src}, D{dst_dreg}"),
            Self::Abcd { src, dst } => write!(f, "ABCD {src}, {dst}"),
            Self::Exg { src, dst } => write!(f, "EXG {src}, {dst}"),
            Self::And { size, src, dst } => write!(f, "AND.{size} {src}, {dst}"),
            Self::Add { size, src, dst } => write!(f, "ADD.{size} {src}, {dst}"),
            Self::Addx { src, dst, size } => write!(f, "ADDX.{size} {src}, {dst}"),
            Self::Adda(dst_areg, size, src) => write!(f, "ADDA.{size} {src}, A{dst_areg}"),
            Self::Shift(typ, size, dir, rot, dst) => {
                write!(f, "{typ}{dir}.{size} ")?;
                match dst {
                    EffectiveAddress::DataReg(_) => write!(f, "{rot}, {dst}"),
                    _ => write!(f, "{dst}"),
                }
            }
        }
    }
}
