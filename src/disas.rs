use std::fmt::Display;

use bitvec::prelude::*;

use crate::instruction::{
    BitInsType, Condition, EffectiveAddress, Instruction, MoveDirection, RegisterEffective,
    Rotation, ShiftDirection, ShiftType, Size,
};

use derive_try_from_primitive::TryFromPrimitive;

#[derive(TryFromPrimitive, Debug, Copy, Clone, PartialEq, Eq)]
#[repr(u8)]
enum InstructionCategory {
    BitMovepImmed = 0,
    MoveByte = 1,
    MoveLong = 2,
    MoveWord = 3,
    Misc = 4,
    AddqSubqSccDbcc = 5,
    Branch = 6,
    Moveq = 7,
    OrDivSbcd = 8,
    SubSubx = 9,
    CmpEor = 11,
    AndMulAbcdExg = 12,
    AddAddx = 13,
    ShiftRotate = 14,
}

#[derive(TryFromPrimitive, Debug, Copy, Clone, PartialEq, Eq)]
#[repr(u8)]
enum AddressingMode {
    DataReg = 0,
    AddressReg = 1,
    Address = 2,
    AdddresPostinc = 3,
    AddressPredec = 4,
    AddressDisplacement = 5,
    AddressIndex = 6,
    Misc = 7,
}

#[derive(TryFromPrimitive, Debug, Copy, Clone, PartialEq, Eq)]
#[repr(u8)]
enum MiscMode {
    PcDisplacement = 2,
    PcIndex = 3,
    AbsoluteShort = 0,
    AbsoluteLong = 1,
    Immediate = 4,
}

pub fn disasm<T>(
    pc: u32,
    byte_read: &mut dyn FnMut(u32) -> Result<u8, T>,
) -> Result<(Instruction, u32), DisassemblyError<T>> {
    let mut disasm = Disasm { pc, byte_read };
    Ok((disasm.disasm()?, disasm.pc))
}

#[derive(Debug)]
pub enum DisassemblyError<T> {
    InvalidInstruction,
    ReadError(u32, T),
}

impl<T: Display> Display for DisassemblyError<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidInstruction => f.write_str("Invalid instruction"),
            Self::ReadError(addr, e) => f.write_fmt(format_args!("Read error at {} ({})", addr, e)),
        }
    }
}

struct Disasm<'a, T> {
    pc: u32,
    byte_read: &'a mut dyn FnMut(u32) -> Result<u8, T>,
}

impl<T> Disasm<'_, T> {
    fn disasm(&mut self) -> Result<Instruction, DisassemblyError<T>> {
        let ins_word = self.read_prog_word()?;
        let ins_word = ins_word.view_bits::<Msb0>();
        let category = InstructionCategory::try_from(ins_word[0..4].load_be::<u8>())
            .map_err(|_| DisassemblyError::InvalidInstruction)?;
        let size: Size = match category {
            InstructionCategory::BitMovepImmed => {
                if ins_word[7] {
                    if ins_word[10..13].load_be::<u8>() == 1 {
                        if ins_word[9] {
                            Size::Long
                        } else {
                            Size::Word
                        }
                    } else if ins_word[10..13].load_be::<u8>() == 0 {
                        Size::Long
                    } else {
                        Size::Byte
                    }
                } else if ins_word[4..7].load_be::<u8>() <= 6 {
                    match ins_word[8..10].load_be::<u8>() {
                        0 => Size::Byte,
                        1 => Size::Word,
                        2 => Size::Long,
                        _ => return Err(DisassemblyError::InvalidInstruction),
                    }
                } else if ins_word[10..13].load_be::<u8>() == 0 {
                    Size::Long
                } else {
                    Size::Byte
                }
            }
            InstructionCategory::MoveByte => Size::Byte,
            InstructionCategory::MoveWord => Size::Word,
            InstructionCategory::MoveLong | InstructionCategory::Moveq => Size::Long,
            InstructionCategory::Misc => {
                if ins_word[7..10].load_be::<u8>() == 7 {
                    Size::Long
                } else if !ins_word[4] {
                    match ins_word[8..10].load_be::<u8>() {
                        0 => Size::Byte,
                        1 => Size::Word,
                        2 => Size::Long,
                        3 => match ins_word[5..7].load_be::<u8>() {
                            0 | 3 => Size::Word,
                            1 => return Err(DisassemblyError::InvalidInstruction),
                            2 => Size::Byte,
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    }
                } else if !ins_word[6] {
                    if ins_word[8] {
                        if ins_word[9] {
                            Size::Long
                        } else {
                            Size::Word
                        }
                    } else if !ins_word[9] {
                        Size::Byte
                    } else if ins_word[9] & (ins_word[10..13].load_be::<u8>() == 0) {
                        Size::Word
                    } else {
                        Size::Long
                    }
                } else if !ins_word[5] {
                    match ins_word[8..10].load_be::<u8>() {
                        0 | 3 => Size::Byte,
                        1 => Size::Word,
                        2 => Size::Long,
                        _ => unreachable!(),
                    }
                } else if ins_word[8] {
                    if ins_word[7..10].load_be::<u8>() == 7 {
                        Size::Long
                    } else if ins_word[7..10].load_be::<u8>() == 6 {
                        Size::Word
                    } else if !ins_word[6] {
                        if ins_word[9] {
                            Size::Long
                        } else {
                            Size::Word
                        }
                    } else {
                        Size::Byte
                    }
                } else if ins_word[9..12].load_be::<u8>() == 5 {
                    Size::Word
                } else {
                    Size::Long
                }
            }
            InstructionCategory::AddqSubqSccDbcc => match ins_word[8..10].load_be::<u8>() {
                0 => Size::Byte,
                1 => Size::Word,
                2 => Size::Long,
                3 => match ins_word[10..13].load_be::<u8>() {
                    1 => Size::Word,
                    _ => Size::Byte,
                },
                _ => unreachable!(),
            },
            InstructionCategory::Branch => {
                if ins_word[8..16].load_be::<i8>() == 0 {
                    Size::Word
                } else {
                    Size::Byte
                }
            }
            InstructionCategory::OrDivSbcd => {
                if ins_word[10..12].load_be::<u8>() == 0 {
                    Size::Byte
                } else {
                    match ins_word[8..10].load_be::<u8>() {
                        0 => Size::Byte,
                        1 | 3 => Size::Word,
                        2 => Size::Long,
                        _ => unreachable!(),
                    }
                }
            }
            InstructionCategory::AndMulAbcdExg => match ins_word[8..10].load_be::<u8>() {
                0 => Size::Byte,
                1 | 2 => Size::Long,
                3 => Size::Word,
                _ => unreachable!(),
            },
            InstructionCategory::AddAddx
            | InstructionCategory::SubSubx
            | InstructionCategory::CmpEor => match ins_word[8..10].load_be::<u8>() {
                0 => Size::Byte,
                1 => Size::Word,
                2 => Size::Long,
                3 => {
                    if ins_word[7] {
                        Size::Long
                    } else {
                        Size::Word
                    }
                }
                _ => unreachable!(),
            },
            InstructionCategory::ShiftRotate => match ins_word[8..10].load_be::<u8>() {
                0 => Size::Byte,
                1 | 3 => Size::Word,
                2 => Size::Long,
                _ => unreachable!(),
            },
        };
        match category {
            InstructionCategory::BitMovepImmed => {
                if ins_word[7] {
                    if ins_word[10..13].load_be::<u8>() == 1 {
                        let dir = if ins_word[8] {
                            MoveDirection::RegToMem
                        } else {
                            MoveDirection::MemToReg
                        };
                        let areg = ins_word[13..16].load_be::<u8>();
                        let dreg = ins_word[4..7].load_be::<u8>();
                        let start_offset = self.read_prog_word()? as i16;
                        Ok(Instruction::Movep {
                            dir,
                            size,
                            areg,
                            dreg,
                            start_offset,
                        })
                    } else {
                        let dst_mode =
                            AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                        let dst_reg = ins_word[13..16].load_be::<u8>();
                        let dst = self.decode_effective(dst_mode, dst_reg, size)?;
                        let idx = EffectiveAddress::DataReg(ins_word[4..7].load_be::<u8>());
                        let typ = match ins_word[8..10].load_be::<u8>() {
                            0 => BitInsType::Test,
                            1 => BitInsType::Change,
                            2 => BitInsType::Clear,
                            3 => BitInsType::Set,
                            _ => unreachable!(),
                        };
                        Ok(Instruction::Bit {
                            typ,
                            idx,
                            dst,
                            size,
                        })
                    }
                } else {
                    match ins_word[4..7].load_be::<u8>() {
                        0 => {
                            let imm = self.read_immediate(size)?;
                            let dst_mode =
                                AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                            let dst_reg = ins_word[13..16].load_be::<u8>();
                            if (dst_mode == AddressingMode::Misc) & (dst_reg == 0b100) {
                                if size == Size::Byte {
                                    Ok(Instruction::OriCcr(imm as u8))
                                } else {
                                    Ok(Instruction::OriSr(imm as u16))
                                }
                            } else {
                                let dst = self.decode_effective(dst_mode, dst_reg, size)?;
                                Ok(Instruction::Ori(size, dst, imm))
                            }
                        }
                        1 => {
                            let imm = self.read_immediate(size)?;
                            let dst_mode =
                                AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                            let dst_reg = ins_word[13..16].load_be::<u8>();
                            if (dst_mode == AddressingMode::Misc) & (dst_reg == 0b100) {
                                if size == Size::Byte {
                                    Ok(Instruction::AndiCcr(imm as u8))
                                } else {
                                    Ok(Instruction::AndiSr(imm as u16))
                                }
                            } else {
                                let dst = self.decode_effective(dst_mode, dst_reg, size)?;
                                Ok(Instruction::Andi(size, dst, imm))
                            }
                        }
                        2 => {
                            let imm = self.read_immediate(size)?;
                            let dst_mode =
                                AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                            let dst_reg = ins_word[13..16].load_be::<u8>();
                            let dst = self.decode_effective(dst_mode, dst_reg, size)?;
                            Ok(Instruction::Subi(size, dst, imm))
                        }
                        3 => {
                            let imm = self.read_immediate(size)?;
                            let dst_mode =
                                AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                            let dst_reg = ins_word[13..16].load_be::<u8>();
                            let dst = self.decode_effective(dst_mode, dst_reg, size)?;
                            Ok(Instruction::Addi(size, dst, imm))
                        }
                        4 => {
                            let dst_mode =
                                AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                            let dst_reg = ins_word[13..16].load_be::<u8>();
                            let dst = self.decode_effective(dst_mode, dst_reg, size)?;
                            let idx = self.read_immediate(Size::Byte)?;
                            let typ = match ins_word[8..10].load_be::<u8>() {
                                0 => BitInsType::Test,
                                1 => BitInsType::Change,
                                2 => BitInsType::Clear,
                                3 => BitInsType::Set,
                                _ => unreachable!(),
                            };
                            Ok(Instruction::Bit {
                                typ,
                                idx: EffectiveAddress::Immediate(idx),
                                dst,
                                size,
                            })
                        }
                        5 => {
                            let imm = self.read_immediate(size)?;
                            let dst_mode =
                                AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                            let dst_reg = ins_word[13..16].load_be::<u8>();
                            if (dst_mode == AddressingMode::Misc) & (dst_reg == 0b100) {
                                if size == Size::Byte {
                                    Ok(Instruction::EoriCcr(imm as u8))
                                } else {
                                    Ok(Instruction::EoriSr(imm as u16))
                                }
                            } else {
                                let dst = self.decode_effective(dst_mode, dst_reg, size)?;
                                Ok(Instruction::Eori(size, dst, imm))
                            }
                        }
                        6 => {
                            let imm = self.read_immediate(size)?;
                            let dst_mode =
                                AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                            let dst_reg = ins_word[13..16].load_be::<u8>();
                            let dst = self.decode_effective(dst_mode, dst_reg, size)?;
                            Ok(Instruction::Cmpi(size, dst, imm))
                        }
                        7 => Err(DisassemblyError::InvalidInstruction),
                        _ => unreachable!(),
                    }
                }
            }
            InstructionCategory::MoveByte
            | InstructionCategory::MoveWord
            | InstructionCategory::MoveLong => {
                let src_mode = AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                let src_reg = ins_word[13..16].load_be::<u8>();
                let src = self.decode_effective(src_mode, src_reg, size)?;
                let dst_mode = AddressingMode::try_from(ins_word[7..10].load_be::<u8>()).unwrap();
                let dst_reg = ins_word[4..7].load_be::<u8>();
                let dst = self.decode_effective(dst_mode, dst_reg, size)?;
                Ok(Instruction::Move { src, dst, size })
            }
            InstructionCategory::Misc => {
                if ins_word[4..10].load_be::<u8>() == 0b00_0011 {
                    let dst_mode =
                        AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                    let dst_reg = ins_word[13..16].load_be::<u8>();
                    let dst = self.decode_effective(dst_mode, dst_reg, size)?;
                    Ok(Instruction::MoveFromSr(dst))
                } else if ins_word[4..10].load_be::<u8>() == 0b01_0011 {
                    let src_mode =
                        AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                    let src_reg = ins_word[13..16].load_be::<u8>();
                    let src = self.decode_effective(src_mode, src_reg, size)?;
                    Ok(Instruction::MoveToCcr(src))
                } else if ins_word[4..10].load_be::<u8>() == 0b01_1011 {
                    let src_mode =
                        AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                    let src_reg = ins_word[13..16].load_be::<u8>();
                    let src = self.decode_effective(src_mode, src_reg, size)?;
                    Ok(Instruction::MoveToSr(src))
                } else if ins_word[4..8].load_be::<u8>() == 0b0000 {
                    let dst_mode =
                        AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                    let dst_reg = ins_word[13..16].load_be::<u8>();
                    let dst = self.decode_effective(dst_mode, dst_reg, size)?;
                    Ok(Instruction::Negx(size, dst))
                } else if ins_word[4..8].load_be::<u8>() == 0b0010 {
                    let dst_mode =
                        AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                    let dst_reg = ins_word[13..16].load_be::<u8>();
                    let dst = self.decode_effective(dst_mode, dst_reg, size)?;
                    Ok(Instruction::Clr(size, dst))
                } else if ins_word[4..8].load_be::<u8>() == 0b0100 {
                    let dst_mode =
                        AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                    let dst_reg = ins_word[13..16].load_be::<u8>();
                    let dst = self.decode_effective(dst_mode, dst_reg, size)?;
                    Ok(Instruction::Neg(size, dst))
                } else if ins_word[4..8].load_be::<u8>() == 0b0110 {
                    let dst_mode =
                        AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                    let dst_reg = ins_word[13..16].load_be::<u8>();
                    let dst = self.decode_effective(dst_mode, dst_reg, size)?;
                    Ok(Instruction::Not(size, dst))
                } else if ins_word[4..9].load_be::<u8>() == 0b10001
                    && ins_word[10..13].load_be::<u8>() == 0
                {
                    let dst = ins_word[13..16].load_be::<u8>();
                    Ok(Instruction::Ext(size, dst))
                } else if ins_word[4..10].load_be::<u8>() == 0b10_0000 {
                    let dst_mode =
                        AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                    let dst_reg = ins_word[13..16].load_be::<u8>();
                    let dst = self.decode_effective(dst_mode, dst_reg, size)?;
                    Ok(Instruction::Nbcd(dst))
                } else if ins_word[4..10].load_be::<u8>() == 0b10_0001 {
                    if ins_word[10..13].load_be::<u8>() == 0 {
                        let dst = ins_word[13..16].load_be::<u8>();
                        Ok(Instruction::Swap(dst))
                    } else {
                        let src_mode =
                            AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                        let src_reg = ins_word[13..16].load_be::<u8>();
                        let src = self.decode_effective(src_mode, src_reg, Size::Byte)?;
                        Ok(Instruction::Pea(src))
                    }
                } else if ins_word[4..10].load_be::<u8>() == 0b10_1011 {
                    let dst_mode =
                        AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                    let dst_reg = ins_word[13..16].load_be::<u8>();
                    let dst = self.decode_effective(dst_mode, dst_reg, Size::Byte)?;
                    Ok(Instruction::Tas(dst))
                } else if ins_word[4..8].load_be::<u8>() == 0b1010 {
                    let dst_mode =
                        AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                    let dst_reg = ins_word[13..16].load_be::<u8>();
                    let dst = self.decode_effective(dst_mode, dst_reg, Size::Byte)?;
                    Ok(Instruction::Tst(size, dst))
                } else if ins_word[4..12].load_be::<u16>() == 0b1110_0100 {
                    let vector = ins_word[12..16].load_be::<u8>() + 32;
                    Ok(Instruction::Trap(vector))
                } else if ins_word[4..13].load_be::<u16>() == 0b1_1100_1010 {
                    let areg = ins_word[13..16].load_be::<u8>();
                    let displacement = self.read_immediate(Size::Word)? as u16;
                    Ok(Instruction::Link { areg, displacement })
                } else if ins_word[4..13].load_be::<u16>() == 0b1_1100_1011 {
                    let dst = ins_word[13..16].load_be::<u8>();
                    Ok(Instruction::Unlk(dst))
                } else if ins_word[4..12].load_be::<u8>() == 0b1110_0110 {
                    let areg = ins_word[13..16].load_be::<u8>();
                    let dir = if ins_word[12] {
                        MoveDirection::MemToReg
                    } else {
                        MoveDirection::RegToMem
                    };
                    Ok(Instruction::MoveUsp(dir, areg))
                } else if ins_word[4..16].load_be::<u16>() == 0b1110_0111_0000 {
                    Ok(Instruction::Reset)
                } else if ins_word[4..16].load_be::<u16>() == 0b1110_0111_0001 {
                    Ok(Instruction::Nop)
                } else if ins_word[4..16].load_be::<u16>() == 0b1110_0111_0010 {
                    let sr = self.read_immediate(Size::Word)? as u16;
                    Ok(Instruction::Stop(sr))
                } else if ins_word[4..16].load_be::<u16>() == 0b1110_0111_0011 {
                    Ok(Instruction::Rte)
                } else if ins_word[4..16].load_be::<u16>() == 0b1110_0111_0101 {
                    Ok(Instruction::Rts)
                } else if ins_word[4..16].load_be::<u16>() == 0b1110_0111_0110 {
                    Ok(Instruction::Trapv)
                } else if ins_word[4..16].load_be::<u16>() == 0b1110_0111_0111 {
                    Ok(Instruction::Rtr)
                } else if ins_word[4..10].load_be::<u8>() == 0b11_1010 {
                    let dst_mode =
                        AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                    let dst_reg = ins_word[13..16].load_be::<u8>();
                    let dst = self.decode_effective(dst_mode, dst_reg, Size::Long)?;
                    Ok(Instruction::Jsr(dst))
                } else if ins_word[4..10].load_be::<u8>() == 0b11_1011 {
                    let dst_mode =
                        AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                    let dst_reg = ins_word[13..16].load_be::<u8>();
                    let dst = self.decode_effective(dst_mode, dst_reg, Size::Long)?;
                    Ok(Instruction::Jmp(dst))
                } else if ins_word[4] && ins_word[6..9].load_be::<u8>() == 0b001 {
                    let dir = if ins_word[5] {
                        MoveDirection::MemToReg
                    } else {
                        MoveDirection::RegToMem
                    };
                    let mode = AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                    let reg = ins_word[13..16].load_be::<u8>();
                    let ea = self.decode_effective(mode, reg, Size::Byte)?;
                    let reg_mask = self.read_prog_word()?;
                    let reg_mask = BitSlice::<u16, Lsb0>::from_element(&reg_mask);
                    let mut regs: Vec<EffectiveAddress> = Vec::new();
                    if mode == AddressingMode::AddressPredec {
                        for (i, bit) in reg_mask.iter().enumerate() {
                            if *bit {
                                if i < 8 {
                                    let reg_no = 7 - i;
                                    regs.push(EffectiveAddress::AddressReg(reg_no as u8));
                                } else {
                                    let reg_no = 7 - (i - 8);
                                    regs.push(EffectiveAddress::DataReg(reg_no as u8));
                                }
                            }
                        }
                    } else {
                        for (i, bit) in reg_mask.iter().enumerate() {
                            if *bit {
                                if i < 8 {
                                    regs.push(EffectiveAddress::DataReg(i as u8));
                                } else {
                                    regs.push(EffectiveAddress::AddressReg((i - 8) as u8));
                                }
                            }
                        }
                    }
                    Ok(Instruction::Movem(dir, size, ea, regs))
                } else if ins_word[7..10].load_be::<u8>() == 0b111 {
                    let src_mode =
                        AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                    let src_reg = ins_word[13..16].load_be::<u8>();
                    let src = self.decode_effective(src_mode, src_reg, Size::Byte)?;
                    let areg = ins_word[4..7].load_be::<u8>();
                    Ok(Instruction::Lea(areg, src))
                } else if ins_word[7..10].load_be::<u8>() == 0b110 {
                    let upper_bound_mode =
                        AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                    let upper_bound_reg = ins_word[13..16].load_be::<u8>();
                    let upper_bound =
                        self.decode_effective(upper_bound_mode, upper_bound_reg, Size::Byte)?;
                    let dreg = ins_word[4..7].load_be::<u8>();
                    Ok(Instruction::Chk(dreg, upper_bound))
                } else {
                    Err(DisassemblyError::InvalidInstruction)
                }
            }
            InstructionCategory::AddqSubqSccDbcc => {
                if ins_word[8..10].load_be::<u8>() == 3 {
                    let condition = Condition::try_from(ins_word[4..8].load_be::<u8>()).unwrap();
                    let dst_mode =
                        AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                    let dst_reg = ins_word[13..16].load_be::<u8>();
                    let dst = self.decode_effective(dst_mode, dst_reg, size)?;
                    if ins_word[10..13].load_be::<u8>() == 1 {
                        let displacement = self.read_immediate(Size::Word)? as i16;
                        let pc = self.pc.wrapping_sub(2);
                        let new_pc = if displacement >= 0 {
                            pc.wrapping_add(displacement as u32)
                        } else {
                            pc.wrapping_sub(-displacement as u32)
                        };
                        Ok(Instruction::Dbcc(condition, dst_reg, displacement, new_pc))
                    } else {
                        Ok(Instruction::Scc(condition, dst))
                    }
                } else {
                    let dst_mode =
                        AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                    let dst_reg = ins_word[13..16].load_be::<u8>();
                    let dst = self.decode_effective(dst_mode, dst_reg, size)?;
                    let src = ins_word[4..7].load_be::<u8>() as i8;
                    if ins_word[7] {
                        Ok(Instruction::Subq(size, src, dst))
                    } else {
                        Ok(Instruction::Addq(size, src, dst))
                    }
                }
            }
            InstructionCategory::Branch => {
                let condition = Condition::try_from(ins_word[4..8].load_be::<u8>()).unwrap();
                let displacement = match size {
                    Size::Word => self.read_immediate(size)? as i16,
                    Size::Byte => i16::from(ins_word[8..16].load_be::<i8>()),
                    Size::Long => unreachable!(),
                };
                let pc = match size {
                    Size::Word => self.pc - 2,
                    Size::Byte => self.pc,
                    Size::Long => unreachable!(),
                };
                let new_pc = if displacement >= 0 {
                    pc.wrapping_add(displacement as u16 as u32)
                } else {
                    pc.wrapping_sub(-displacement as u16 as u32)
                };
                if condition == Condition::False {
                    Ok(Instruction::Bsr(size, displacement, new_pc))
                } else if condition == Condition::True {
                    Ok(Instruction::Bra(size, displacement, new_pc))
                } else {
                    Ok(Instruction::Bcc(size, condition, displacement, new_pc))
                }
            }
            InstructionCategory::Moveq => {
                let dreg = ins_word[4..7].load_be::<u8>();
                let imm = ins_word[8..16].load_be::<u8>() as i8;
                Ok(Instruction::Moveq { dreg, imm })
            }
            InstructionCategory::OrDivSbcd =>
            {
                #[allow(clippy::if_same_then_else)]
                if ins_word[8..10].load_be::<u8>() == 3 {
                    let src_mode =
                        AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                    let src_reg = ins_word[13..16].load_be::<u8>();
                    let src = self.decode_effective(src_mode, src_reg, size)?;
                    let dst = ins_word[4..7].load_be::<u8>();
                    if ins_word[7] {
                        Ok(Instruction::Divs(dst, src))
                    } else {
                        Ok(Instruction::Divu(dst, src))
                    }
                } else if ins_word[7..12].load_be::<u8>() == 0b1_0000 {
                    let mode = if ins_word[12] {
                        AddressingMode::AddressPredec
                    } else {
                        AddressingMode::DataReg
                    };
                    let src_reg = ins_word[13..16].load_be::<u8>();
                    let src = self.decode_effective(mode, src_reg, size)?;
                    let dst_reg = ins_word[4..7].load_be::<u8>();
                    let dst = self.decode_effective(mode, dst_reg, size)?;
                    Ok(Instruction::Sbcd { src, dst })
                } else {
                    let dreg = EffectiveAddress::DataReg(ins_word[4..7].load_be::<u8>());
                    let ea_mode =
                        AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                    let ea_reg = ins_word[13..16].load_be::<u8>();
                    let ea = self.decode_effective(ea_mode, ea_reg, size)?;
                    let (src, dst) = if ins_word[7] { (dreg, ea) } else { (ea, dreg) };
                    Ok(Instruction::Or { size, src, dst })
                }
            }
            InstructionCategory::SubSubx => {
                let src;
                let dst;
                if ins_word[8..10].load_be::<u8>() == 3 {
                    let src = self.decode_effective(
                        AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap(),
                        ins_word[13..16].load_be::<u8>(),
                        size,
                    )?;
                    let areg = ins_word[4..7].load_be::<u8>();
                    Ok(Instruction::Suba(areg, size, src))
                } else {
                    let dir = ins_word[7];
                    if dir & (ins_word[10..12].load_be::<u8>() == 0) {
                        let mode = if ins_word[12] {
                            AddressingMode::AddressPredec
                        } else {
                            AddressingMode::DataReg
                        };
                        src =
                            self.decode_effective(mode, ins_word[13..16].load_be::<u8>(), size)?;
                        dst = self.decode_effective(mode, ins_word[4..7].load_be::<u8>(), size)?;
                        Ok(Instruction::Subx { size, src, dst })
                    } else {
                        let am_1 =
                            AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                        let reg_1 = ins_word[13..16].load_be::<u8>();
                        let ea_1 = self.decode_effective(am_1, reg_1, size)?;
                        let dreg = EffectiveAddress::DataReg(ins_word[4..7].load_be::<u8>());
                        if dir {
                            Ok(Instruction::Sub {
                                size,
                                src: dreg,
                                dst: ea_1,
                            })
                        } else {
                            Ok(Instruction::Sub {
                                size,
                                src: ea_1,
                                dst: dreg,
                            })
                        }
                    }
                }
            }
            InstructionCategory::CmpEor => {
                if ins_word[8..10].load_be::<u8>() == 3 {
                    let src_mode =
                        AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                    let src_reg = ins_word[13..16].load_be::<u8>();
                    let src = self.decode_effective(src_mode, src_reg, size)?;
                    let dst = ins_word[4..7].load_be::<u8>();
                    Ok(Instruction::Cmpa(dst, size, src))
                } else if ins_word[7] {
                    #[allow(clippy::if_same_then_else)]
                    if ins_word[10..13].load_be::<u8>() == 1 {
                        let dst = ins_word[4..7].load_be::<u8>();
                        let src = ins_word[13..16].load_be::<u8>();
                        Ok(Instruction::Cmpm { size, src, dst })
                    } else {
                        let src = EffectiveAddress::DataReg(ins_word[4..7].load_be::<u8>());
                        let dst_mode =
                            AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                        let dst_reg = ins_word[13..16].load_be::<u8>();
                        let dst = self.decode_effective(dst_mode, dst_reg, size)?;
                        Ok(Instruction::Eor { size, src, dst })
                    }
                } else {
                    let src_mode =
                        AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                    let src_reg = ins_word[13..16].load_be::<u8>();
                    let src = self.decode_effective(src_mode, src_reg, size)?;
                    let dst = ins_word[4..7].load_be::<u8>();
                    Ok(Instruction::Cmp(dst, size, src))
                }
            }
            InstructionCategory::AndMulAbcdExg => match ins_word[8..10].load_be::<u8>() {
                3 => {
                    let src_mode =
                        AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                    let src_reg = ins_word[13..16].load_be::<u8>();
                    let src = self.decode_effective(src_mode, src_reg, size)?;
                    let dst = ins_word[7..10].load_be::<u8>();
                    if ins_word[7] {
                        Ok(Instruction::Muls(dst, src))
                    } else {
                        Ok(Instruction::Mulu(dst, src))
                    }
                }
                0 => {
                    let mode = if ins_word[12] {
                        AddressingMode::AddressPredec
                    } else {
                        AddressingMode::DataReg
                    };
                    let src_reg = ins_word[13..16].load_be::<u8>();
                    let src = self.decode_effective(mode, src_reg, size)?;
                    let dst_reg = ins_word[4..7].load_be::<u8>();
                    let dst = self.decode_effective(mode, dst_reg, size)?;
                    Ok(Instruction::Abcd { src, dst })
                }
                x @ (1 | 2) => {
                    if ins_word[7] && ins_word[10..12].load_be::<u8>() == 0 {
                        let (mode1, mode2) = match x << 1 | u8::from(ins_word[12]) {
                            0b010 => (AddressingMode::DataReg, AddressingMode::DataReg),
                            0b011 => (AddressingMode::AddressReg, AddressingMode::AddressReg),
                            0b101 => (AddressingMode::DataReg, AddressingMode::AddressReg),
                            _ => return Err(DisassemblyError::InvalidInstruction),
                        };
                        let reg1 = ins_word[4..7].load_be::<u8>();
                        let reg2 = ins_word[13..16].load_be::<u8>();
                        let ea1 = self.decode_effective(mode1, reg1, Size::Long)?;
                        let ea2 = self.decode_effective(mode2, reg2, Size::Long)?;
                        Ok(Instruction::Exg { src: ea1, dst: ea2 })
                    } else {
                        let dreg = EffectiveAddress::DataReg(ins_word[4..7].load_be::<u8>());
                        let ea_mode =
                            AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                        let ea_reg = ins_word[13..16].load_be::<u8>();
                        let ea = self.decode_effective(ea_mode, ea_reg, size)?;
                        let (src, dst) = if ins_word[7] { (dreg, ea) } else { (ea, dreg) };
                        Ok(Instruction::And { size, src, dst })
                    }
                }
                _ => unreachable!(),
            },
            InstructionCategory::AddAddx => {
                if ins_word[8..10].load_be::<u8>() == 3 {
                    let src = self.decode_effective(
                        AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap(),
                        ins_word[13..16].load_be::<u8>(),
                        size,
                    )?;
                    let areg = ins_word[4..7].load_be::<u8>();
                    Ok(Instruction::Adda(areg, size, src))
                } else {
                    let dir = ins_word[7];
                    if dir & (ins_word[10..12].load_be::<u8>() == 0) {
                        let mode = if ins_word[12] {
                            AddressingMode::AddressPredec
                        } else {
                            AddressingMode::DataReg
                        };
                        let src =
                            self.decode_effective(mode, ins_word[13..16].load_be::<u8>(), size)?;
                        let dst =
                            self.decode_effective(mode, ins_word[7..10].load_be::<u8>(), size)?;
                        Ok(Instruction::Addx { size, src, dst })
                    } else {
                        let am_1 =
                            AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                        let reg_1 = ins_word[13..16].load_be::<u8>();
                        let ea_1 = self.decode_effective(am_1, reg_1, size)?;
                        let dreg = EffectiveAddress::DataReg(ins_word[4..7].load_be::<u8>());
                        if dir {
                            Ok(Instruction::Add {
                                size,
                                src: dreg,
                                dst: ea_1,
                            })
                        } else {
                            Ok(Instruction::Add {
                                size,
                                src: ea_1,
                                dst: dreg,
                            })
                        }
                    }
                }
            }
            InstructionCategory::ShiftRotate => {
                let dir = if ins_word[7] {
                    ShiftDirection::Left
                } else {
                    ShiftDirection::Right
                };
                let (op, dst, rotation) = if ins_word[8..10].load_be::<u8>() == 3 {
                    let op = ins_word[5..7].load_be::<u8>();
                    let dst_mode =
                        AddressingMode::try_from(ins_word[10..13].load_be::<u8>()).unwrap();
                    let dst_reg = ins_word[13..16].load_be::<u8>();
                    let dst = self.decode_effective(dst_mode, dst_reg, size)?;
                    (op, dst, Rotation::Immediate(1))
                } else {
                    let op = ins_word[11..13].load_be::<u8>();
                    let dst = EffectiveAddress::DataReg(ins_word[13..16].load_be::<u8>());
                    let rotation = if ins_word[10] {
                        Rotation::Register(ins_word[4..7].load_be::<u8>())
                    } else {
                        Rotation::Immediate(ins_word[4..7].load_be::<u8>())
                    };
                    (op, dst, rotation)
                };
                let op = match op {
                    0 => ShiftType::Arithmetic,
                    1 => ShiftType::Logical,
                    2 => ShiftType::RotateExtend,
                    3 => ShiftType::Rotate,
                    _ => unreachable!(),
                };
                Ok(Instruction::Shift(op, size, dir, rotation, dst))
            }
        }
    }

    fn decode_effective(
        &mut self,
        mode: AddressingMode,
        reg: u8,
        size: Size,
    ) -> Result<EffectiveAddress, DisassemblyError<T>> {
        if matches!(mode, AddressingMode::Misc) {
            let misc_mode = MiscMode::try_from(reg).unwrap();
            match misc_mode {
                MiscMode::PcDisplacement => {
                    let pc = self.pc;
                    Ok(EffectiveAddress::PcDisplacement(pc, self.read_prog_word()?))
                }
                MiscMode::PcIndex => {
                    let pc = self.pc;
                    let (idx, idx_size, displacement) = self.read_ext_word()?;
                    Ok(EffectiveAddress::PcIndex(pc, displacement, idx, idx_size))
                }
                MiscMode::AbsoluteShort => Ok(EffectiveAddress::AbsoluteShort(i32::from(
                    self.read_prog_word()? as i16,
                )
                    as u16)),
                MiscMode::AbsoluteLong => Ok(EffectiveAddress::AbsoluteLong(
                    self.read_immediate(Size::Long)?,
                )),
                MiscMode::Immediate => Ok(EffectiveAddress::Immediate(self.read_immediate(size)?)),
            }
        } else {
            match mode {
                AddressingMode::DataReg => Ok(EffectiveAddress::DataReg(reg)),
                AddressingMode::AddressReg => Ok(EffectiveAddress::AddressReg(reg)),
                AddressingMode::Address => Ok(EffectiveAddress::Address(reg)),
                AddressingMode::AdddresPostinc => Ok(EffectiveAddress::AddressPostinc(reg)),
                AddressingMode::AddressPredec => Ok(EffectiveAddress::AddressPredec(reg)),
                AddressingMode::AddressDisplacement => Ok(EffectiveAddress::AddressDisplacement(
                    reg,
                    self.read_prog_word()? as i16,
                )),
                AddressingMode::AddressIndex => {
                    let (idx, idx_size, displacement) = self.read_ext_word()?;
                    Ok(EffectiveAddress::AddressIndex {
                        reg,
                        displacement,
                        idx,
                        idx_size,
                    })
                }

                AddressingMode::Misc => unreachable!(),
            }
        }
    }

    fn read_ext_word(&mut self) -> Result<(RegisterEffective, Size, u8), DisassemblyError<T>> {
        let ext_word = self.read_prog_word()?;
        let ext_word = ext_word.view_bits::<Msb0>();
        let mode = AddressingMode::try_from(ext_word[0..1].load_be::<u8>()).unwrap();
        let reg = ext_word[1..4].load_be::<u8>();
        let size = if ext_word[4] { Size::Long } else { Size::Word };
        let ea = RegisterEffective::try_from(self.decode_effective(mode, reg, size)?).unwrap();
        let displacement = ext_word[8..16].load_be::<u8>();
        Ok((ea, size, displacement))
    }

    fn read_prog_word(&mut self) -> Result<u16, DisassemblyError<T>> {
        let word = ((self.byte_read)(self.pc)
            .map_err(|e| DisassemblyError::ReadError(self.pc, e))? as u16)
            << 8
            | ((self.byte_read)(self.pc + 1)
                .map_err(|e| DisassemblyError::ReadError(self.pc + 1, e))? as u16);
        self.pc += 2;
        Ok(word)
    }

    fn read_immediate(&mut self, size: Size) -> Result<u32, DisassemblyError<T>> {
        match size {
            Size::Byte => Ok(u32::from(self.read_prog_word()? & 0xFF)),
            Size::Word => Ok(u32::from(self.read_prog_word()?)),
            Size::Long => {
                Ok((u32::from(self.read_prog_word()?) << 16) | u32::from(self.read_prog_word()?))
            }
        }
    }
}
