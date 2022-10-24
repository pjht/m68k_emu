use bitvec::prelude::*;
use std::{
    error::Error,
    fmt::{Debug, Display},
};

use crate::{
    backplane::Backplane,
    disas::{self, DisassemblyError},
    instruction::{
        ArithType, BitInsType, EffectiveAddress, Instruction, MoveDirection, ShiftDirection,
        ShiftType, Size,
    },
};

#[derive(Debug)]
pub struct BusError;

impl Display for BusError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Bus error")
    }
}

impl Error for BusError {}

// pub trait Bus: Debug {
//     fn read_word(&mut self, address: u32) -> Result<u16, BusError>;
//     fn read_byte(&mut self, address: u32) -> Result<u8, BusError>;
//     fn write_word(&mut self, address: u32, data: u16) -> Result<(), BusError>;
//     fn write_byte(&mut self, address: u32, data: u8) -> Result<(), BusError>;
// }

#[derive(Debug)]
pub struct M68K {
    dregs: [u32; 8],
    aregs: [u32; 7],
    usp: u32,
    ssp: u32,
    pc: u32,
    sr: u16,
    bus: Backplane,
    pub stopped: bool,
    initialized: bool,
}

impl Display for M68K {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_supervisor() {
            f.write_str("Mode: Supervisor\n")?;
        } else {
            f.write_str("Mode: User\n")?;
        }
        for (i, val) in self.dregs[0..4].iter().enumerate() {
            f.write_fmt(format_args!("D{}: 0x{:0>8x} ", i, val))?;
        }
        f.write_str("\n")?;
        for (i, val) in self.dregs[4..8].iter().enumerate() {
            f.write_fmt(format_args!("D{}: 0x{:0>8x} ", i + 4, val))?;
        }
        f.write_str("\n")?;
        for (i, val) in self.aregs[0..4].iter().enumerate() {
            f.write_fmt(format_args!("A{}: 0x{:0>8x} ", i, val))?;
        }
        f.write_str("\n")?;
        for (i, val) in self.aregs[4..7].iter().enumerate() {
            f.write_fmt(format_args!("A{}: 0x{:0>8x} ", i + 4, val))?;
        }
        if self.is_supervisor() {
            f.write_fmt(format_args!("A7: 0x{:0>8x}\n", self.ssp))?;
            f.write_fmt(format_args!("USP: 0x{:0>8x}", self.usp))?;
        } else {
            f.write_fmt(format_args!("A7: 0x{:0>8x}\n", self.usp))?;
            f.write_fmt(format_args!("SSP: 0x{:0>8x}", self.ssp))?;
        }
        Ok(())
    }
}

impl M68K {
    pub fn new(bus: Backplane) -> Self {
        Self {
            dregs: [0; 8],
            aregs: [0; 7],
            usp: 0,
            ssp: 0,
            pc: 0,
            sr: 0,
            bus,
            stopped: false,
            initialized: false,
        }
    }

    pub fn reset(&mut self) {
        let ssp_high = self.read_word(0);
        let ssp_low = self.read_word(2);
        self.ssp = ((ssp_high as u32) << 16) | (ssp_low as u32);
        let pc_high = self.read_word(4);
        let pc_low = self.read_word(6);
        self.pc = (u32::from(pc_high) << 16) | u32::from(pc_low);
        self.sr = 0x2700 | self.sr & 0xFF;
        self.stopped = false;
        self.initialized = true;
    }

    pub fn disassemble(
        &mut self,
        loc: u32,
    ) -> Result<(Instruction, u32), DisassemblyError<BusError>> {
        disas::disasm(loc, &mut |addr| self.bus.read_byte(addr))
    }

    pub fn step(&mut self) {
        if self.stopped {
            return;
        }
        if !self.initialized {
            self.reset();
        }
        let (ins, new_pc) = match self.disassemble(self.pc) {
            Ok(ins) => ins,
            Err(DisassemblyError::InvalidInstruction) => panic!("Invalid instruction"),
            Err(DisassemblyError::ReadError(addr, _e)) => {
                panic!("Bus error while reading instruction at address {addr:#x}")
            }
        };
        self.pc = new_pc;
        match ins {
            Instruction::OriCcr(val) => {
                self.sr = (self.sr & 0xFF00) | ((self.sr & 0xFF) | val as u16);
            }
            Instruction::OriSr(val) => self.sr |= val as u16,
            Instruction::Ori(size, dst, val) => {
                let dst_val = self.read_effective(dst, size);
                let res = dst_val | val;
                self.write_effective(dst, res, size);
                let zero = res == 0;
                let neg = match size {
                    Size::Byte => (res & 0x80) > 0,
                    Size::Word => (res & 0x8000) > 0,
                    Size::Long => (res & 0x8000_0000) > 0,
                };
                let old_flags = self.sr & 0xFFE0;
                let new_flags = (old_flags & 0x10) | u16::from(neg) << 3 | u16::from(zero) << 2;
                self.sr = (self.sr & 0xFFE0) | new_flags;
            }
            Instruction::AndiCcr(val) => {
                self.sr = (self.sr & 0xFF00) | ((self.sr & 0xFF) & val as u16);
            }
            Instruction::AndiSr(val) => self.sr &= val as u16,
            Instruction::Andi(size, dst, val) => {
                let dst_val = self.read_effective(dst, size);
                let res = dst_val & val;
                self.write_effective(dst, res, size);
                let zero = res == 0;
                let neg = match size {
                    Size::Byte => (res & 0x80) > 0,
                    Size::Word => (res & 0x8000) > 0,
                    Size::Long => (res & 0x8000_0000) > 0,
                };
                let old_flags = self.sr & 0xFFE0;
                let new_flags = (old_flags & 0x10) | u16::from(neg) << 3 | u16::from(zero) << 2;
                self.sr = (self.sr & 0xFFE0) | new_flags;
            }
            Instruction::Subi(size, dst, val) => {
                self.sub(dst, EffectiveAddress::Immediate(val), size, ArithType::Reg);
            }
            Instruction::Addi(size, dst, val) => {
                self.add(dst, EffectiveAddress::Immediate(val), size, ArithType::Reg);
            }
            Instruction::EoriCcr(val) => {
                self.sr = (self.sr & 0xFF00) | ((self.sr & 0xFF) ^ val as u16);
            }
            Instruction::EoriSr(val) => self.sr ^= val as u16,
            Instruction::Eori(size, dst, val) => {
                let dst_val = self.read_effective(dst, size);
                let res = dst_val ^ val;
                self.write_effective(dst, res, size);
                let zero = res == 0;
                let neg = match size {
                    Size::Byte => (res & 0x80) > 0,
                    Size::Word => (res & 0x8000) > 0,
                    Size::Long => (res & 0x8000_0000) > 0,
                };
                let old_flags = self.sr & 0xFFE0;
                let new_flags = (old_flags & 0x10) | u16::from(neg) << 3 | u16::from(zero) << 2;
                self.sr = (self.sr & 0xFFE0) | new_flags;
            }
            Instruction::Cmpi(size, dst, val) => {
                self.cmp(dst, EffectiveAddress::Immediate(val), size);
            }
            Instruction::Bit {
                typ,
                idx,
                dst,
                size,
            } => {
                let mut dst_val = self.read_effective(dst, size);
                let idx = self.read_effective(idx, Size::Byte) as usize;
                let dst_bits = BitSlice::<u32, Lsb0>::from_element_mut(&mut dst_val);
                let dst_bit = dst_bits[idx];
                self.sr = (self.sr & 0xFFFB) | (u16::from(dst_bit) << 2);
                match typ {
                    BitInsType::Test => (),
                    BitInsType::Change => dst_bits.set(idx, !dst_bit),
                    BitInsType::Clear => dst_bits.set(idx, false),
                    BitInsType::Set => dst_bits.set(idx, true),
                }
                self.write_effective(dst, dst_val, size);
            }
            Instruction::Movep {
                dir,
                size,
                dreg,
                areg,
                start_offset,
            } => match dir {
                MoveDirection::MemToReg => match size {
                    Size::Byte => unreachable!(),
                    Size::Word => {
                        let high = self.read_effective(
                            EffectiveAddress::AddressDisplacement(areg, start_offset),
                            Size::Byte,
                        ) as u8;
                        let low = self.read_effective(
                            EffectiveAddress::AddressDisplacement(areg, start_offset + 2),
                            Size::Byte,
                        ) as u8;
                        self.write_effective(
                            EffectiveAddress::DataReg(dreg),
                            ((high as u32) << 8) | (low as u32),
                            Size::Word,
                        );
                    }
                    Size::Long => {
                        let high = self.read_effective(
                            EffectiveAddress::AddressDisplacement(areg, start_offset),
                            Size::Byte,
                        ) as u8;
                        let mid_high = self.read_effective(
                            EffectiveAddress::AddressDisplacement(areg, start_offset + 2),
                            Size::Byte,
                        ) as u8;
                        let mid_low = self.read_effective(
                            EffectiveAddress::AddressDisplacement(areg, start_offset + 4),
                            Size::Byte,
                        ) as u8;
                        let low = self.read_effective(
                            EffectiveAddress::AddressDisplacement(areg, start_offset + 6),
                            Size::Byte,
                        ) as u8;
                        self.write_effective(
                            EffectiveAddress::DataReg(dreg),
                            ((high as u32) << 24)
                                | ((mid_high as u32) << 16)
                                | ((mid_low as u32) << 8)
                                | (low as u32),
                            Size::Word,
                        );
                    }
                },
                MoveDirection::RegToMem => match size {
                    Size::Byte => unreachable!(),
                    Size::Word => {
                        self.write_effective(
                            EffectiveAddress::AddressDisplacement(areg, start_offset),
                            self.dregs[dreg as usize] & 0xFF,
                            Size::Byte,
                        );
                        self.write_effective(
                            EffectiveAddress::AddressDisplacement(areg, start_offset + 2),
                            (self.dregs[dreg as usize] & 0xFF00) >> 8,
                            Size::Byte,
                        );
                    }
                    Size::Long => {
                        self.write_effective(
                            EffectiveAddress::AddressDisplacement(areg, start_offset),
                            self.dregs[dreg as usize] & 0xFF,
                            Size::Byte,
                        );
                        self.write_effective(
                            EffectiveAddress::AddressDisplacement(areg, start_offset + 2),
                            (self.dregs[dreg as usize] & 0xFF00) >> 8,
                            Size::Byte,
                        );
                        self.write_effective(
                            EffectiveAddress::AddressDisplacement(areg, start_offset + 4),
                            (self.dregs[dreg as usize] & 0xFF_0000) >> 16,
                            Size::Byte,
                        );
                        self.write_effective(
                            EffectiveAddress::AddressDisplacement(areg, start_offset + 6),
                            (self.dregs[dreg as usize] & 0xFF00_0000) >> 24,
                            Size::Byte,
                        );
                    }
                },
            },
            Instruction::Move { src, dst, size } => {
                let src_val = self.read_effective(src, size);
                self.write_effective(dst, src_val, size);
                if !matches!(dst, EffectiveAddress::AddressReg(_)) {
                    let zero = src_val == 0;
                    let neg = match size {
                        Size::Byte => (src_val & 0x80) > 0,
                        Size::Word => (src_val & 0x8000) > 0,
                        Size::Long => (src_val & 0x8000_0000) > 0,
                    };
                    let old_flags = self.sr & 0xFFE0;
                    let new_flags = (old_flags & 0x10) | u16::from(neg) << 3 | u16::from(zero) << 2;
                    self.sr = (self.sr & 0xFFE0) | new_flags;
                }
            }
            Instruction::MoveFromSr(dst) => {
                self.write_effective(dst, u32::from(self.sr), Size::Word);
            }
            Instruction::MoveToCcr(src) => {
                let src_val = self.read_effective(src, Size::Byte);
                self.sr = (self.sr & 0xFF00) | src_val as u16;
            }
            Instruction::MoveToSr(src) => {
                let src_val = self.read_effective(src, Size::Word);
                self.sr = src_val as u16;
            }
            Instruction::Negx(size, dst) => {
                self.sub(dst, EffectiveAddress::Immediate(0), size, ArithType::Ext);
            }
            Instruction::Clr(size, dst) => {
                self.write_effective(dst, 0, size);
                self.sr = (self.sr & 0xFFF0) | 0b0100;
            }
            Instruction::Neg(size, dst) => {
                self.sub(dst, EffectiveAddress::Immediate(0), size, ArithType::Reg);
            }
            Instruction::Not(size, dst) => {
                let dst_val = self.read_effective(dst, size);
                let res = Self::trim_excess(!dst_val, size);
                let zero = res == 0;
                let neg = match size {
                    Size::Byte => (res & 0x80) > 0,
                    Size::Word => (res & 0x8000) > 0,
                    Size::Long => (res & 0x8000_0000) > 0,
                };
                self.write_effective(dst, res, size);
                self.sr = (self.sr & 0xFFF0) | u16::from(neg) << 3 | u16::from(zero) << 2;
            }
            Instruction::Ext(size, dreg) => {
                let dst = EffectiveAddress::DataReg(dreg);
                let dst_val = self.read_effective(dst, Size::Byte);
                let res = i32::from(dst_val as u8 as i8) as u32;
                let zero = res == 0;
                let neg = match size {
                    Size::Byte => (res & 0x80) > 0,
                    Size::Word => (res & 0x8000) > 0,
                    Size::Long => (res & 0x8000_0000) > 0,
                };
                self.sr = (self.sr & 0xFFF0) | u16::from(neg) << 3 | u16::from(zero) << 2;
                self.write_effective(dst, res, size);
            }
            Instruction::Nbcd(_) => todo!(),
            Instruction::Swap(dreg) => {
                let dst = EffectiveAddress::DataReg(dreg);
                let dst_val = self.read_effective(dst, Size::Long);
                let res = (dst_val & 0xFFFF) << 16 | (dst_val & 0xFFFF_0000);
                let zero = res == 0;
                let neg = (res & 0x8000_0000) > 0;
                self.sr = (self.sr & 0xFFF0) | u16::from(neg) << 3 | u16::from(zero) << 2;
                self.write_effective(dst, res, Size::Long);
            }
            Instruction::Pea(ea) => {
                let ea = self.effective_address(ea);
                self.push(ea, Size::Long);
            }
            Instruction::Tas(dst) => {
                let dst_val = self.read_effective(dst, Size::Byte);
                let neg = (dst_val & 0x80) > 0;
                let zero = dst_val == 0;
                let old_flags = self.sr & 0xFFE0;
                let new_flags = (old_flags & 0x10) | u16::from(neg) << 3 | u16::from(zero) << 2;
                self.write_effective(dst, dst_val | 0x80, Size::Byte);
                self.sr = (self.sr & 0xFFE0) | new_flags;
            }
            Instruction::Tst(size, src) => {
                let dst_val = self.read_effective(src, Size::Byte);
                let neg = match size {
                    Size::Byte => (dst_val & 0x80) > 0,
                    Size::Word => (dst_val & 0x8000) > 0,
                    Size::Long => (dst_val & 0x8000_0000) > 0,
                };
                let zero = dst_val == 0;
                let old_flags = self.sr & 0xFFE0;
                let new_flags = (old_flags & 0x10) | u16::from(neg) << 3 | u16::from(zero) << 2;
                self.sr = (self.sr & 0xFFE0) | new_flags;
            }
            Instruction::Trap(vec) => self.trap(vec),
            Instruction::Link { areg, displacement } => {
                let dst = EffectiveAddress::AddressReg(areg);
                let dst_val = self.read_effective(dst, Size::Long);
                self.push(dst_val, Size::Long);
                let sp = self.read_effective(EffectiveAddress::AddressReg(7), Size::Long);
                self.write_effective(dst, sp, Size::Long);
                self.write_effective(
                    EffectiveAddress::AddressReg(7),
                    sp.wrapping_add(u32::from(displacement)),
                    Size::Long,
                );
            }
            Instruction::Unlk(areg) => {
                let dst = EffectiveAddress::AddressReg(areg);
                let dst_val = self.read_effective(dst, Size::Long);
                self.write_effective(EffectiveAddress::AddressReg(7), dst_val, Size::Long);
                let old_dst = self.pop(Size::Long);
                self.write_effective(dst, old_dst, Size::Long);
            }
            Instruction::MoveUsp(dir, areg) => {
                let areg = EffectiveAddress::AddressReg(areg);
                match dir {
                    MoveDirection::MemToReg => self.write_effective(areg, self.usp, Size::Long),
                    MoveDirection::RegToMem => self.usp = self.read_effective(areg, Size::Long),
                }
            }
            Instruction::Reset => self.reset(),
            Instruction::Nop => (),
            Instruction::Stop(sr) => {
                self.sr = sr;
                self.stopped = true;
            }
            Instruction::Rte => {
                self.sr = self.pop(Size::Word) as u16;
                self.pc = self.pop(Size::Long);
                self.pop(Size::Long); //Discard format + vector offset
            }
            Instruction::Rts => self.pc = self.pop(Size::Long),
            Instruction::Trapv => {
                if self.sr & 0x2 > 0 {
                    self.trap(7);
                }
            }
            Instruction::Rtr => {
                self.sr = (self.sr & 0xFF00) | u16::from(self.pop(Size::Word) as u8);
                self.pc = self.pop(Size::Long);
            }
            Instruction::Jsr(ea) => {
                self.push(self.pc, Size::Long);
                self.pc = self.effective_address(ea);
            }
            Instruction::Jmp(ea) => self.pc = self.effective_address(ea),
            Instruction::Movem(dir, size, dst, regs) => match dir {
                MoveDirection::MemToReg => {
                    for &reg in &regs {
                        let ea_val = self.read_effective(dst, size);
                        self.write_effective(reg, ea_val, size);
                    }
                }
                MoveDirection::RegToMem => {
                    for &reg in &regs {
                        let reg_val = self.read_effective(reg, size);
                        self.write_effective(dst, reg_val, size);
                    }
                }
            },
            Instruction::Lea(areg, ea) => {
                let ea = self.effective_address(ea);
                self.write_effective(EffectiveAddress::AddressReg(areg), ea, Size::Long);
            }
            Instruction::Chk(src, bound) => {
                let src = EffectiveAddress::DataReg(src);
                let upper_bound_val = self.read_effective(bound, Size::Word) as u16 as i16;
                let dreg_val = self.read_effective(src, Size::Word) as u16 as i16;
                if dreg_val < 0 || dreg_val > upper_bound_val {
                    self.trap(6);
                }
            }
            Instruction::Addq(size, val, dst) => self.add(
                dst,
                EffectiveAddress::Immediate(val as i32 as u32),
                size,
                if matches!(dst, EffectiveAddress::AddressReg(_)) {
                    ArithType::Addr
                } else {
                    ArithType::Reg
                },
            ),
            Instruction::Subq(size, val, dst) => self.sub(
                dst,
                EffectiveAddress::Immediate(val as i32 as u32),
                size,
                if matches!(dst, EffectiveAddress::AddressReg(_)) {
                    ArithType::Addr
                } else {
                    ArithType::Reg
                },
            ),
            Instruction::Scc(cond, dst) => self.write_effective(
                dst,
                if cond.matches(self.sr as u8) { 0xFF } else { 0 },
                Size::Byte,
            ),
            Instruction::Dbcc(cond, dreg, _disp, new_pc) => {
                let dst = EffectiveAddress::DataReg(dreg);
                if cond.matches(self.sr as u8) {
                    return;
                }
                let dst_val = self.read_effective(dst, Size::Word);
                let res = dst_val.wrapping_sub(1) as u16;
                self.write_effective(dst, u32::from(res), Size::Word);
                if res == 0xFFFF {
                    return;
                }
                self.pc = new_pc;
            }
            Instruction::Bra(_size, _disp, new_pc) => self.pc = new_pc,
            Instruction::Bsr(_size, _disp, new_pc) => {
                self.push(self.pc, Size::Long);
                self.pc = new_pc;
            }
            Instruction::Bcc(_size, cond, _disp, new_pc) => {
                if cond.matches(self.sr as u8) {
                    self.pc = new_pc;
                }
            }
            Instruction::Moveq { dreg, imm } => {
                let imm = imm as i32 as u32;
                let neg = (imm & 0x8000_0000) > 0;
                let zero = imm == 0;
                let old_flags = self.sr & 0xFFE0;
                let new_flags = (old_flags & 0x10) | u16::from(neg) << 3 | u16::from(zero) << 2;
                self.sr = (self.sr & 0xFFE0) | new_flags;
                self.write_effective(EffectiveAddress::DataReg(dreg), imm, Size::Long);
            }
            Instruction::Divu(dst_dreg, src) => {
                let dst = EffectiveAddress::DataReg(dst_dreg);
                let src_val = self.read_effective(src, Size::Word);
                let dst_val = self.read_effective(dst, Size::Word);
                if src_val == 0 {
                    self.trap(5);
                }
                let dst_val = dst_val as i32;
                let src_val = i32::from(src_val as u16 as i16);
                let quot = dst_val / src_val;
                let rem = dst_val % src_val;
                self.write_effective(
                    dst,
                    (rem as i16 as u32) << 16 | (quot as i16 as u32) & 0xFFFF,
                    Size::Long,
                );
                if i16::try_from(quot).is_err() || i16::try_from(rem).is_err() {
                    self.sr = (self.sr & 0xFFF0) | 0b0010;
                } else {
                    let zero = quot == 0;
                    let neg = quot < 0;
                    self.sr = (self.sr & 0xFFF0) | (u16::from(neg) << 3) | (u16::from(zero) << 2);
                }
            }
            Instruction::Divs(dst_dreg, src) => {
                let dst = EffectiveAddress::DataReg(dst_dreg);
                let src_val = self.read_effective(src, Size::Word);
                let dst_val = self.read_effective(dst, Size::Word);
                if src_val == 0 {
                    self.trap(5);
                }
                let quot = dst_val / src_val;
                let rem = dst_val % src_val;
                self.write_effective(dst, rem << 16 | quot & 0xFFFF, Size::Long);
                if quot > u32::from(u16::MAX) || rem > u32::from(u16::MAX) {
                    self.sr = (self.sr & 0xFFF0) | 0b0010;
                } else {
                    let zero = quot == 0;
                    self.sr = (self.sr & 0xFFF0) | (u16::from(zero) << 2);
                }
            }
            Instruction::Sbcd { .. } => todo!(),
            Instruction::Or { size, src, dst } => {
                let src_val = self.read_effective(src, size);
                let dst_val = self.read_effective(dst, size);
                let res = dst_val | src_val;
                self.write_effective(dst, res, size);
                let zero = res == 0;
                let neg = match size {
                    Size::Byte => (res & 0x80) > 0,
                    Size::Word => (res & 0x8000) > 0,
                    Size::Long => (res & 0x8000_0000) > 0,
                };
                let old_flags = self.sr & 0xFFE0;
                let new_flags = (old_flags & 0x10) | u16::from(neg) << 3 | u16::from(zero) << 2;
                self.sr = (self.sr & 0xFFE0) | new_flags;
            }
            Instruction::Sub { size, src, dst } => self.sub(dst, src, size, ArithType::Reg),
            Instruction::Subx { src, dst, size } => self.sub(dst, src, size, ArithType::Ext),
            Instruction::Suba(dst_areg, size, src) => self.sub(
                EffectiveAddress::AddressReg(dst_areg),
                src,
                size,
                ArithType::Addr,
            ),
            Instruction::Eor { size, src, dst } => {
                let src_val = self.read_effective(src, size);
                let dst_val = self.read_effective(dst, size);
                let res = dst_val ^ src_val;
                self.write_effective(dst, res, size);
                let zero = res == 0;
                let neg = match size {
                    Size::Byte => (res & 0x80) > 0,
                    Size::Word => (res & 0x8000) > 0,
                    Size::Long => (res & 0x8000_0000) > 0,
                };
                let old_flags = self.sr & 0xFFE0;
                let new_flags = (old_flags & 0x10) | u16::from(neg) << 3 | u16::from(zero) << 2;
                self.sr = (self.sr & 0xFFE0) | new_flags;
            }
            Instruction::Cmpm { size, src, dst } => {
                let src = EffectiveAddress::AddressPostinc(src);
                let dst = EffectiveAddress::AddressPostinc(dst);
                self.cmp(dst, src, size);
            }
            Instruction::Cmp(dst_dreg, size, src) => {
                self.cmp(EffectiveAddress::DataReg(dst_dreg), src, size);
            }
            Instruction::Cmpa(dst_areg, size, src) => {
                let dst = EffectiveAddress::AddressReg(dst_areg);
                let src_val = self.read_effective(src, size);
                let dst_val = self.read_effective(dst, size);
                let cmp_src = match size {
                    Size::Word => i32::from(src_val as u16 as i16) as u32,
                    Size::Long => src_val,
                    Size::Byte => unreachable!(),
                };
                let cmp_dst = match size {
                    Size::Word => i32::from(dst_val as u16 as i16) as u32,
                    Size::Long => dst_val,
                    Size::Byte => unreachable!(),
                };
                self.cmp(
                    EffectiveAddress::Immediate(cmp_dst),
                    EffectiveAddress::Immediate(cmp_src),
                    size,
                );
            }
            Instruction::Mulu(dst_dreg, src) => {
                let dst = EffectiveAddress::DataReg(dst_dreg);
                let src_val = self.read_effective(src, Size::Word);
                let dst_val = self.read_effective(dst, Size::Word);

                let res = src_val * dst_val;
                self.write_effective(dst, res, Size::Long);
                let neg = (res & 0x8000_0000) > 0;
                let zero = res == 0;
                let old_flags = self.sr & 0xFFE0;
                let new_flags = (old_flags & 0x10) | u16::from(neg) << 3 | u16::from(zero) << 2;
                self.sr = (self.sr & 0xFFE0) | new_flags;
            }
            Instruction::Muls(dst_dreg, src) => {
                let dst = EffectiveAddress::DataReg(dst_dreg);
                let src_val = self.read_effective(src, Size::Word);
                let dst_val = self.read_effective(dst, Size::Word);
                let res = (src_val as i32 * dst_val as i32) as u32;
                self.write_effective(dst, res, Size::Long);
                let neg = (res & 0x8000_0000) > 0;
                let zero = res == 0;
                let old_flags = self.sr & 0xFFE0;
                let new_flags = (old_flags & 0x10) | u16::from(neg) << 3 | u16::from(zero) << 2;
                self.sr = (self.sr & 0xFFE0) | new_flags;
            }
            Instruction::Abcd { .. } => todo!(),

            Instruction::Exg { src, dst } => {
                let src_val = self.read_effective(src, Size::Long);
                let dst_val = self.read_effective(dst, Size::Long);
                self.write_effective(src, dst_val, Size::Long);
                self.write_effective(dst, src_val, Size::Long);
            }
            Instruction::And { size, src, dst } => {
                let src_val = self.read_effective(src, size);
                let dst_val = self.read_effective(dst, size);
                let res = dst_val & src_val;
                self.write_effective(dst, res, size);
                let zero = res == 0;
                let neg = match size {
                    Size::Byte => (res & 0x80) > 0,
                    Size::Word => (res & 0x8000) > 0,
                    Size::Long => (res & 0x8000_0000) > 0,
                };
                let old_flags = self.sr & 0xFFE0;
                let new_flags = (old_flags & 0x10) | u16::from(neg) << 3 | u16::from(zero) << 2;
                self.sr = (self.sr & 0xFFE0) | new_flags;
            }
            Instruction::Add { size, src, dst } => self.add(dst, src, size, ArithType::Reg),
            Instruction::Addx { src, dst, size } => self.add(dst, src, size, ArithType::Ext),
            Instruction::Adda(dst_areg, size, src) => self.add(
                EffectiveAddress::AddressReg(dst_areg),
                src,
                size,
                ArithType::Addr,
            ),
            Instruction::Shift(typ, size, dir, rot, dst) => {
                let dst_val = self.read_effective(dst, size);
                let rotation = match rot {
                    crate::instruction::Rotation::Immediate(rot) => rot,
                    crate::instruction::Rotation::Register(dreg) => {
                        self.read_effective(EffectiveAddress::DataReg(dreg), Size::Byte) as u8
                    }
                };
                let (carry, res): (bool, u32) = match (typ, dir) {
                    (ShiftType::Arithmetic, ShiftDirection::Right) => {
                        let carry = (dst_val & (1 << (rotation - 1))) > 0;
                        let res = match size {
                            Size::Byte => ((dst_val as u8 as i8) >> rotation) as u32,
                            Size::Word => ((dst_val as u16 as i16) >> rotation) as u32,
                            Size::Long => ((dst_val as i32) >> rotation) as u32,
                        };
                        (carry, res)
                    }
                    (ShiftType::Arithmetic | ShiftType::Logical, ShiftDirection::Left) => {
                        let max_bit = match size {
                            Size::Byte => 7,
                            Size::Word => 15,
                            Size::Long => 31,
                        };
                        let carry_bit_num = max_bit - (rotation - 1);
                        let carry = (dst_val & (1 << carry_bit_num)) > 0;
                        (carry, dst_val << rotation)
                    }
                    (ShiftType::Logical, ShiftDirection::Right) => {
                        let carry = (dst_val & (1 << (rotation - 1))) > 0;
                        (carry, dst_val >> rotation)
                    }
                    (ShiftType::RotateExtend, ShiftDirection::Right) => todo!(),
                    (ShiftType::RotateExtend, ShiftDirection::Left) => todo!(),
                    (ShiftType::Rotate, ShiftDirection::Right) => {
                        let max_bit = match size {
                            Size::Byte => 7,
                            Size::Word => 15,
                            Size::Long => 31,
                        };
                        let carry = (dst_val & (1 << ((rotation % (max_bit + 1)) - 1))) > 0;
                        let res = match size {
                            Size::Byte => {
                                u32::from((dst_val as u8).rotate_right(u32::from(rotation)))
                            }
                            Size::Word => {
                                u32::from((dst_val as u16).rotate_right(u32::from(rotation)))
                            }
                            Size::Long => dst_val.rotate_right(u32::from(rotation)),
                        };
                        (carry, res)
                    }
                    (ShiftType::Rotate, ShiftDirection::Left) => {
                        let max_bit = match size {
                            Size::Byte => 7,
                            Size::Word => 15,
                            Size::Long => 31,
                        };
                        let carry_bit_num = max_bit - ((rotation % (max_bit + 1)) - 1);
                        let carry = (dst_val & (1 << carry_bit_num)) > 0;
                        let res = match size {
                            Size::Byte => {
                                u32::from((dst_val as u8).rotate_left(u32::from(rotation)))
                            }
                            Size::Word => {
                                u32::from((dst_val as u16).rotate_left(u32::from(rotation)))
                            }
                            Size::Long => dst_val.rotate_left(u32::from(rotation)),
                        };
                        (carry, res)
                    }
                };
                let zero = res == 0;
                let neg = match size {
                    Size::Byte => (res & 0x80) > 0,
                    Size::Word => (res & 0x8000) > 0,
                    Size::Long => (res & 0x8000_0000) > 0,
                };
                self.write_effective(dst, res, size);
                let flags = u8::from(carry) << 4
                    | u8::from(neg) << 3
                    | u8::from(zero) << 2
                    | u8::from(carry);
                self.sr = self.sr & 0xFFE0 | u16::from(flags);
            }
        }
    }

    fn add(
        &mut self,
        dst: EffectiveAddress,
        src: EffectiveAddress,
        mut size: Size,
        typ: ArithType,
    ) {
        let ext = match typ {
            ArithType::Ext => (self.sr & 0x0010) > 0,
            _ => false,
        };
        let mut src_val = self.read_effective(src, size);
        let dst_val = self.read_effective(dst, size);
        let res;
        let carry;
        let ovf;
        let zero;
        let neg;
        if typ == ArithType::Addr && size == Size::Word {
            src_val = i32::from(src_val as u16 as i16) as u32;
            size = Size::Long;
        };
        match size {
            Size::Byte => {
                let src_val = src_val as u8;
                let dst_val = dst_val as u8;
                let res8;
                (res8, carry) = dst_val.carrying_add(src_val, ext);
                res = u32::from(res8);
                (_, ovf) = (dst_val as i8).overflowing_add(src_val as i8);
                zero = res == 0;
                neg = res & 0x80 > 0;
            }
            Size::Word => {
                let src_val = src_val as u16;
                let dst_val = dst_val as u16;
                let res16;
                (res16, carry) = dst_val.carrying_add(src_val, ext);
                res = u32::from(res16);
                (_, ovf) = (dst_val as i16).overflowing_add(src_val as i16);
                zero = res == 0;
                neg = res & 0x8000 > 0;
            }
            Size::Long => {
                (res, carry) = dst_val.carrying_add(src_val, ext);
                (_, ovf) = (dst_val as i32).overflowing_add(src_val as i32);
                zero = res == 0;
                neg = res & 0x8000_0000 > 0;
            }
        }
        self.write_effective(dst, res, size);
        match typ {
            ArithType::Reg => {
                let flags = u8::from(carry) << 4
                    | u8::from(neg) << 3
                    | u8::from(zero) << 2
                    | u8::from(ovf) << 1
                    | u8::from(carry);
                self.sr = self.sr & 0xFFE0 | u16::from(flags);
            }
            ArithType::Addr => (),
            ArithType::Ext => {
                let prev_flags = (self.sr & 0x1F) as u8;
                let prev_zero = (prev_flags & 0x4 >> 2) > 0;
                let new_zero = prev_zero & zero;
                let flags = u8::from(carry) << 4
                    | u8::from(neg) << 3
                    | u8::from(new_zero) << 2
                    | u8::from(ovf) << 1
                    | u8::from(carry);
                self.sr = self.sr & 0xFFE0 | u16::from(flags);
            }
        }
    }

    fn sub(
        &mut self,
        dst: EffectiveAddress,
        src: EffectiveAddress,
        mut size: Size,
        typ: ArithType,
    ) {
        let ext = match typ {
            ArithType::Ext => (self.sr & 0x0010) > 0,
            _ => false,
        };
        let mut src_val = self.read_effective(src, size);
        let dst_val = self.read_effective(dst, size);
        let res;
        let borrow;
        let ovf;
        let zero;
        let neg;
        if typ == ArithType::Addr && size == Size::Word {
            src_val = i32::from(src_val as u16 as i16) as u32;
            size = Size::Long;
        };
        match size {
            Size::Byte => {
                let src_val = src_val as u8;
                let dst_val = dst_val as u8;
                let res8;
                (res8, borrow) = dst_val.borrowing_sub(src_val, ext);
                res = u32::from(res8);
                (_, ovf) = (dst_val as i8).overflowing_sub(src_val as i8);
                zero = res == 0;
                neg = res & 0x80 > 0;
            }
            Size::Word => {
                let src_val = src_val as u16;
                let dst_val = dst_val as u16;
                let res16;
                (res16, borrow) = dst_val.borrowing_sub(src_val, ext);
                res = u32::from(res16);
                (_, ovf) = (dst_val as i16).overflowing_sub(src_val as i16);
                zero = res == 0;
                neg = res & 0x8000 > 0;
            }
            Size::Long => {
                (res, borrow) = dst_val.borrowing_sub(src_val, ext);
                (_, ovf) = (dst_val as i32).overflowing_sub(src_val as i32);
                zero = res == 0;
                neg = res & 0x8000_0000 > 0;
            }
        }
        self.write_effective(dst, res, size);
        match typ {
            ArithType::Reg => {
                let flags = u8::from(borrow) << 4
                    | u8::from(neg) << 3
                    | u8::from(zero) << 2
                    | u8::from(ovf) << 1
                    | u8::from(borrow);
                self.sr = (self.sr & 0xFFE0) | u16::from(flags);
            }
            ArithType::Addr => (),
            ArithType::Ext => {
                let prev_flags = (self.sr & 0x1F) as u8;
                let prev_zero = (prev_flags & 0x4 >> 2) > 0;
                let new_zero = prev_zero & zero;
                let flags = u8::from(borrow) << 4
                    | u8::from(neg) << 3
                    | u8::from(new_zero) << 2
                    | u8::from(ovf) << 1
                    | u8::from(borrow);
                self.sr = (self.sr & 0xFFE0) | u16::from(flags);
            }
        }
    }

    fn cmp(&mut self, dst: EffectiveAddress, src: EffectiveAddress, size: Size) {
        let src_val = self.read_effective(src, size);
        let dst_val = self.read_effective(dst, size);
        let res;
        let borrow;
        let ovf;
        let zero;
        let neg;
        match size {
            Size::Byte => {
                let src_val = src_val as u8;
                let dst_val = dst_val as u8;
                let res8;
                (res8, borrow) = dst_val.borrowing_sub(src_val, false);
                res = u32::from(res8);
                (_, ovf) = (dst_val as i8).overflowing_sub(src_val as i8);
                zero = res == 0;
                neg = res & 0x80 > 0;
            }
            Size::Word => {
                let src_val = src_val as u16;
                let dst_val = dst_val as u16;
                let res16;
                (res16, borrow) = dst_val.borrowing_sub(src_val, false);
                res = u32::from(res16);
                (_, ovf) = (dst_val as i16).overflowing_sub(src_val as i16);
                zero = res == 0;
                neg = res & 0x8000 > 0;
            }
            Size::Long => {
                (res, borrow) = dst_val.borrowing_sub(src_val, false);
                (_, ovf) = (dst_val as i32).overflowing_sub(src_val as i32);
                zero = res == 0;
                neg = res & 0x8000_0000 > 0;
            }
        }
        let flags = u8::from(borrow) << 4
            | u8::from(neg) << 3
            | u8::from(zero) << 2
            | u8::from(ovf) << 1
            | u8::from(borrow);
        self.sr = (self.sr & 0xFFF0) | u16::from(flags);
    }

    fn read_effective(&mut self, effective_address: EffectiveAddress, size: Size) -> u32 {
        match effective_address {
            EffectiveAddress::Immediate(x) => x,
            EffectiveAddress::DataReg(x) => Self::trim_excess(self.dregs[x as usize], size),
            EffectiveAddress::AddressReg(x) => {
                assert!(size != Size::Byte);
                if x == 7 {
                    if self.is_supervisor() {
                        Self::trim_excess(self.ssp, size)
                    } else {
                        Self::trim_excess(self.usp, size)
                    }
                } else {
                    Self::trim_excess(self.aregs[x as usize], size)
                }
            }
            EffectiveAddress::Address(x) => {
                let address = self.read_effective(EffectiveAddress::AddressReg(x), Size::Long);
                self.read_address(address, size)
            }
            EffectiveAddress::AddressPostinc(x) => {
                let mut address = self.read_effective(EffectiveAddress::AddressReg(x), Size::Long);
                let val = self.read_address(address, size);
                address = address.wrapping_add(u32::from(size.byte_size()));
                self.write_effective(EffectiveAddress::AddressReg(x), address, Size::Long);
                val
            }
            EffectiveAddress::AddressPredec(x) => {
                let address = self
                    .read_effective(EffectiveAddress::AddressReg(x), Size::Long)
                    .wrapping_sub(u32::from(size.byte_size()));
                self.write_effective(EffectiveAddress::AddressReg(x), address, Size::Long);
                self.read_address(address, size)
            }
            EffectiveAddress::AddressDisplacement(x, d) => {
                let address = if d > 0 {
                    self.read_effective(EffectiveAddress::AddressReg(x), Size::Long)
                        .wrapping_add((d as u16).into())
                } else {
                    self.read_effective(EffectiveAddress::AddressReg(x), Size::Long)
                        .wrapping_sub((-d as u16).into())
                };
                self.read_address(address, size)
            }
            EffectiveAddress::AddressIndex {
                reg,
                displacement,
                idx,
                idx_size,
            } => {
                let address = self
                    .read_effective(EffectiveAddress::AddressReg(reg), Size::Long)
                    .wrapping_add(displacement.into())
                    .wrapping_add(self.read_effective(EffectiveAddress::from(idx), idx_size));
                self.read_address(address, size)
            }
            EffectiveAddress::PcDisplacement(pc, d) => {
                let address = pc.wrapping_add(d.into());
                self.read_address(address, size)
            }
            EffectiveAddress::PcIndex(pc, displacement, idx, idx_size) => {
                let address = pc
                    .wrapping_add(displacement.into())
                    .wrapping_add(self.read_effective(EffectiveAddress::from(idx), idx_size));
                self.read_address(address, size)
            }
            EffectiveAddress::AbsoluteShort(x) => self.read_address(x as u32, size),
            EffectiveAddress::AbsoluteLong(x) => self.read_address(x, size),
        }
    }

    fn write_effective(&mut self, effective_address: EffectiveAddress, data: u32, size: Size) {
        match effective_address {
            EffectiveAddress::DataReg(x) => {
                self.dregs[x as usize] = Self::set_with_size(self.dregs[x as usize], data, size);
            }
            EffectiveAddress::AddressReg(x) => {
                let data = match size {
                    Size::Byte => unreachable!(),
                    Size::Word => i32::from(data as u16 as i16) as u32,
                    Size::Long => data,
                };
                if x == 7 {
                    if self.is_supervisor() {
                        self.ssp = data;
                    } else {
                        self.usp = data;
                    }
                } else {
                    self.aregs[x as usize] = data;
                }
            }
            EffectiveAddress::Address(x) => {
                let address = self.read_effective(EffectiveAddress::AddressReg(x), Size::Long);
                self.write_address(address, data, size);
            }
            EffectiveAddress::AddressPostinc(x) => {
                let mut address = self.read_effective(EffectiveAddress::AddressReg(x), Size::Long);
                self.write_address(address, data, size);
                if x == 7 && size == Size::Byte {
                    address = address.wrapping_add(2);
                } else {
                    address = address.wrapping_add(u32::from(size.byte_size()));
                }
                self.write_effective(EffectiveAddress::AddressReg(x), address, Size::Long);
            }
            EffectiveAddress::AddressPredec(x) => {
                let mut address = self.read_effective(EffectiveAddress::AddressReg(x), Size::Long);
                if x == 7 && size == Size::Byte {
                    address = address.wrapping_sub(2);
                } else {
                    address = address.wrapping_sub(u32::from(size.byte_size()));
                }
                self.write_effective(EffectiveAddress::AddressReg(x), address, Size::Long);
                self.write_address(address, data, size);
            }
            EffectiveAddress::AddressDisplacement(x, d) => {
                let address = if d > 0 {
                    self.read_effective(EffectiveAddress::AddressReg(x), Size::Long)
                        .wrapping_add((d as u16).into())
                } else {
                    self.read_effective(EffectiveAddress::AddressReg(x), Size::Long)
                        .wrapping_sub((-d as u16).into())
                };
                self.write_address(address, data, size);
            }
            EffectiveAddress::AddressIndex {
                reg,
                displacement,
                idx,
                idx_size,
            } => {
                let address = self
                    .read_effective(EffectiveAddress::AddressReg(reg), Size::Long)
                    .wrapping_add(displacement.into())
                    .wrapping_add(self.read_effective(EffectiveAddress::from(idx), idx_size));
                self.write_address(address, data, size);
            }
            EffectiveAddress::AbsoluteShort(x) => self.write_address(x as u32, data, size),
            EffectiveAddress::AbsoluteLong(x) => self.write_address(x, data, size),
            EffectiveAddress::PcDisplacement(..)
            | EffectiveAddress::PcIndex(..)
            | EffectiveAddress::Immediate(_) => panic!(),
        };
    }

    fn read_address(&mut self, address: u32, size: Size) -> u32 {
        // println!("READ {:x}, {:?}", address, size);
        match size {
            Size::Byte => u32::from(self.read_byte(address)),
            Size::Word => u32::from(self.read_word(address)),
            Size::Long => {
                u32::from(self.read_word(address)) << 16 | u32::from(self.read_word(address + 2))
            }
        }
    }

    fn write_address(&mut self, address: u32, data: u32, size: Size) {
        // println!("WRITE {:x}, {:?}, data {:x}", address, size, data);
        match size {
            Size::Byte => self.write_byte(address, data as u8),
            Size::Word => self.write_word(address, data as u16),
            Size::Long => {
                self.write_word(address, (data >> 16) as u16);
                self.write_word(address + 2, data as u16);
            }
        }
    }

    fn read_byte(&mut self, address: u32) -> u8 {
        self.bus
            .read_byte(address)
            .unwrap_or_else(|_| panic!("Could not read byte from 0x{:0>8x}", address))
    }

    fn write_byte(&mut self, address: u32, data: u8) {
        self.bus
            .write_byte(address, data)
            .unwrap_or_else(|_| panic!("Could not write byte to 0x{:0>8x}", address));
    }

    fn read_word(&mut self, address: u32) -> u16 {
        if address & 0x1 != 0 {
            self.trap(3);
        }
        self.bus
            .read_word(address)
            .unwrap_or_else(|_| panic!("Could not read word from 0x{:0>8x}", address))
    }

    fn write_word(&mut self, address: u32, data: u16) {
        if address & 0x1 != 0 {
            self.trap(3);
        }
        self.bus
            .write_word(address, data)
            .unwrap_or_else(|_| panic!("Could not write word to 0x{:0>8x}", address));
    }

    fn trim_excess(num: u32, size: Size) -> u32 {
        match size {
            Size::Byte => num & 0xFF,
            Size::Word => num & 0xFFFF,
            Size::Long => num,
        }
    }

    fn set_with_size(num: u32, data: u32, size: Size) -> u32 {
        match size {
            Size::Byte => (num & 0xFFFF_FF00) | (data & 0xFF),
            Size::Word => (num & 0xFFFF_0000) | (data & 0xFFFF),
            Size::Long => data,
        }
    }

    fn effective_address(&mut self, ea: EffectiveAddress) -> u32 {
        match ea {
            EffectiveAddress::Address(x) => {
                self.read_effective(EffectiveAddress::AddressReg(x), Size::Long)
            }
            EffectiveAddress::AddressDisplacement(x, d) => {
                if d > 0 {
                    self.read_effective(EffectiveAddress::AddressReg(x), Size::Long)
                        .wrapping_add((d as u16).into())
                } else {
                    self.read_effective(EffectiveAddress::AddressReg(x), Size::Long)
                        .wrapping_sub((-d as u16).into())
                }
            }
            EffectiveAddress::PcDisplacement(pc, d) => pc.wrapping_add(d.into()),
            EffectiveAddress::AbsoluteShort(x) => x as u32,
            EffectiveAddress::AbsoluteLong(x) => x,
            EffectiveAddress::DataReg(_)
            | EffectiveAddress::AddressReg(_)
            | EffectiveAddress::AddressPredec(_)
            | EffectiveAddress::AddressPostinc(_)
            | EffectiveAddress::Immediate(_) => panic!("Invalid type of effective address"),
            EffectiveAddress::AddressIndex {
                reg,
                displacement,
                idx,
                idx_size,
            } => self
                .read_effective(EffectiveAddress::AddressReg(reg), Size::Long)
                .wrapping_add(displacement.into())
                .wrapping_add(self.read_effective(EffectiveAddress::from(idx), idx_size)),
            EffectiveAddress::PcIndex(pc, displacement, idx, idx_size) => pc
                .wrapping_add(displacement.into())
                .wrapping_add(self.read_effective(EffectiveAddress::from(idx), idx_size)),
        }
    }

    fn push(&mut self, data: u32, size: Size) {
        self.write_effective(EffectiveAddress::AddressPredec(7), data, size);
    }

    fn pop(&mut self, size: Size) -> u32 {
        self.read_effective(EffectiveAddress::AddressPostinc(7), size)
    }

    fn is_supervisor(&self) -> bool {
        (self.sr & 0x2000) > 0
    }

    fn trap(&mut self, vector: u8) {
        let new_pc = self.read_address(u32::from(vector) * 4, Size::Long);
        self.push(u32::from(vector), Size::Word);
        self.push(self.pc, Size::Long);
        self.push(u32::from(self.sr), Size::Word);
        self.sr |= 0x2000;
        self.pc = new_pc;
    }

    #[allow(dead_code)]
    pub fn bus(&self) -> &Backplane {
        &self.bus
    }

    pub fn bus_mut(&mut self) -> &mut Backplane {
        &mut self.bus
    }

    pub fn pc(&self) -> u32 {
        self.pc
    }
}
