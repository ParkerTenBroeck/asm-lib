use assembler::{
    Context, NodeRef,
    simple::trans::{
        TranslationUnit, TranslationUnitMachine,
        display::{RightPad, fmt_section_disassembly},
        reloc::Reloc,
        section::Section,
        sym::SymbolIdx,
    },
};

use crate::{
    opcodes::{imm_16, imm_26},
    reg::Register,
};

pub struct MipsTranslationUnit;

impl TranslationUnitMachine for MipsTranslationUnit {
    type Reloc = MipsReloc;
    type PtrSizeType = u32;

    fn fmt_section_disassembly(
        section: &Section<Self>,
        trans: &assembler::simple::trans::TranslationUnit<Self>,
        f: &mut impl std::fmt::Write,
    ) -> std::fmt::Result {
        fmt_section_disassembly(
            section,
            trans,
            f,
            4,
            |_, _, _| 4,
            |_, _, f, ins| {
                let instruction =
                    u32::from_be_bytes(std::array::from_fn(|i| ins.get(i).copied().unwrap_or(0)));

                use std::fmt::Write;
                let mut f = RightPad::new(&mut *f, 32);

                let opcode = (instruction >> 26) & 0x3f;
                let rs = Register(((instruction >> 21) & 0x1f) as u8);
                let rt = Register(((instruction >> 16) & 0x1f) as u8);
                let rd = Register(((instruction >> 11) & 0x1f) as u8);
                let shamt = (instruction >> 6) & 0x1f;
                let funct = instruction & 0x3f;
                let imm = (instruction & 0xffff) as i16 as i32;
                let uimm = (instruction & 0xffff) as u32;
                let addr = instruction & 0x03ff_ffff;

                match opcode {
                    0x00 => match funct {
                        0x20 => write!(f, "add   {}, {}, {}", rd, rs, rt)?,
                        0x21 => write!(f, "addu  {}, {}, {}", rd, rs, rt)?,
                        0x22 => write!(f, "sub   {}, {}, {}", rd, rs, rt)?,
                        0x23 => write!(f, "subu  {}, {}, {}", rd, rs, rt)?,
                        0x24 => write!(f, "and   {}, {}, {}", rd, rs, rt)?,
                        0x27 => write!(f, "nor   {}, {}, {}", rd, rs, rt)?,
                        0x25 => write!(f, "or    {}, {}, {}", rd, rs, rt)?,
                        0x26 => write!(f, "xor   {}, {}, {}", rd, rs, rt)?,

                        0x00 => write!(f, "sll   {}, {}, {}", rd, rt, shamt)?,
                        0x04 => write!(f, "sllv  {}, {}, {}", rd, rt, rs)?,
                        0x03 => write!(f, "sra   {}, {}, {}", rd, rt, shamt)?,
                        0x07 => write!(f, "srav  {}, {}, {}", rd, rt, rs)?,
                        0x02 => write!(f, "srl   {}, {}, {}", rd, rt, shamt)?,
                        0x06 => write!(f, "srlv  {}, {}, {}", rd, rt, rs)?,

                        0x2a => write!(f, "slt   {}, {}, {}", rd, rs, rt)?,
                        0x2b => write!(f, "sltu  {}, {}, {}", rd, rs, rt)?,

                        0x1a => write!(f, "div   {}, {}", rs, rt)?,
                        0x1b => write!(f, "divu  {}, {}", rs, rt)?,
                        0x18 => write!(f, "mult  {}, {}", rs, rt)?,
                        0x19 => write!(f, "multu {}, {}", rs, rt)?,

                        0x10 => write!(f, "mfhi  {}", rd)?,
                        0x12 => write!(f, "mflo  {}", rd)?,
                        0x11 => write!(f, "mthi  {}", rs)?,
                        0x13 => write!(f, "mtlo  {}", rs)?,

                        0x09 => write!(f, "jalr  {}, {}", rd, rs)?,
                        0x08 => write!(f, "jr    {}", rs)?,

                        0x0d => write!(f, "break")?,
                        0x0c => write!(f, "syscall")?,

                        _ => write!(f, "unknown R-type funct=0x{:02x}", funct)?,
                    },

                    0x08 => write!(f, "addi  {}, {}, {}", rt, rs, imm)?,
                    0x09 => write!(f, "addiu {}, {}, {}", rt, rs, imm)?,
                    0x0d => write!(f, "ori   {}, {}, 0x{:x}", rt, rs, uimm)?,
                    0x0e => write!(f, "xori  {}, {}, 0x{:x}", rt, rs, uimm)?,
                    0x0c => write!(f, "andi  {}, {}, 0x{:x}", rt, rs, uimm)?,

                    0x0a => write!(f, "slti  {}, {}, {}", rt, rs, imm)?,
                    0x0b => write!(f, "sltiu {}, {}, {}", rt, rs, imm)?,

                    0x02 => write!(f, "j     0x{:08x}", addr << 2)?,
                    0x03 => write!(f, "jal   0x{:08x}", addr << 2)?,

                    0x04 => write!(f, "beq   {}, {}, {}", rs, rt, imm)?,
                    0x05 => write!(f, "bne   {}, {}, {}", rs, rt, imm)?,
                    0x06 => write!(f, "blez  {}, {}", rs, imm)?,
                    0x07 => write!(f, "bgtz  {}, {}", rs, imm)?,

                    0x01 => match rt.0 {
                        0x00 => write!(f, "bltz   {}, {}", rs, imm)?,
                        0x01 => write!(f, "bgez   {}, {}", rs, imm)?,
                        0x10 => write!(f, "bltzal {}, {}", rs, imm)?,
                        0x11 => write!(f, "bgezal {}, {}", rs, imm)?,
                        _ => write!(f, "unknown REGIMM rt=0x{:02x}", rt.0)?,
                    },

                    0x20 => write!(f, "lb    {}, {}[{}]", rt, imm, rs)?,
                    0x24 => write!(f, "lbu   {}, {}[{}]", rt, imm, rs)?,
                    0x21 => write!(f, "lh    {}, {}[{}]", rt, imm, rs)?,
                    0x25 => write!(f, "lhu   {}, {}[{}]", rt, imm, rs)?,
                    0x23 => write!(f, "lw    {}, {}[{}]", rt, imm, rs)?,
                    0x22 => write!(f, "lwl   {}, {}[{}]", rt, imm, rs)?,
                    0x26 => write!(f, "lwr   {}, {}[{}]", rt, imm, rs)?,

                    0x28 => write!(f, "sb    {}, {}[{}]", rt, imm, rs)?,
                    0x29 => write!(f, "sh    {}, {}[{}]", rt, imm, rs)?,
                    0x2b => write!(f, "sw    {}, {}[{}]", rt, imm, rs)?,
                    0x2a => write!(f, "swl   {}, {}[{}]", rt, imm, rs)?,
                    0x2e => write!(f, "swr   {}, {}[{}]", rt, imm, rs)?,

                    0x0f => write!(f, "lui   {}, 0x{:x}", rt, uimm)?,

                    _ => write!(f, "unknown opcode=0x{:02x}", opcode)?,
                }

                Ok(())
            },
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MipsReloc {
    pub pattern: MipsRelocPattern,
    pub calculation: MipsRelocCalc,
    pub offset: i32,
    pub overflow: bool,
}

impl MipsReloc {
    pub fn with_overflow(
        pattern: MipsRelocPattern,
        calculation: MipsRelocCalc,
        offset: i32,
    ) -> Self {
        Self {
            pattern,
            calculation,
            offset,
            overflow: true,
        }
    }

    pub fn without_overflow(
        pattern: MipsRelocPattern,
        calculation: MipsRelocCalc,
        offset: i32,
    ) -> Self {
        Self {
            pattern,
            calculation,
            offset,
            overflow: false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum MipsRelocPattern {
    JumpU26,
    BranchI16,
    ImmI16,
    ImmU16,
    ImmH16,

    U8,
    U16,
    U32,
}

impl std::fmt::Display for MipsRelocPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MipsRelocPattern::JumpU26 => write!(f, "jump_u26"),
            MipsRelocPattern::BranchI16 => write!(f, "branch_i16"),
            MipsRelocPattern::ImmI16 => write!(f, "imm_i16"),
            MipsRelocPattern::ImmU16 => write!(f, "imm_u16"),
            MipsRelocPattern::ImmH16 => write!(f, "imm_h16"),
            MipsRelocPattern::U8 => write!(f, "u8"),
            MipsRelocPattern::U16 => write!(f, "u16"),
            MipsRelocPattern::U32 => write!(f, "u32"),
        }
    }
}

impl MipsRelocPattern {
    pub fn generate(&self, value: u32) -> u32 {
        match self {
            MipsRelocPattern::JumpU26 => imm_26(value >> 2),
            MipsRelocPattern::BranchI16 => imm_16(value >> 2),
            MipsRelocPattern::ImmI16 => imm_16(value),
            MipsRelocPattern::ImmU16 => imm_16(value),
            MipsRelocPattern::ImmH16 => imm_16(value >> 16),
            MipsRelocPattern::U8 => value & 0xFF,
            MipsRelocPattern::U16 => value & 0xFFFF,
            MipsRelocPattern::U32 => value,
        }
    }
    pub fn checked_unsigned<'a>(
        &self,
        ctx: &mut Context<'a>,
        node: NodeRef<'a>,
        value: u32,
    ) -> u32 {
        match self {
            // Self::JumpU26 if !(0..=0x3FFFFFF).contains(&(value >> 2)) => {
            //     ctx.report_error(
            //         node,
            //         format!("value is out of range for '{self}' relocation: '{value}'"),
            //     );
            // }
            Self::BranchI16
                if !(i16::MAX as i32..=i16::MIN as i32).contains(&(value as i32 >> 2)) =>
            {
                ctx.report_error(
                    node,
                    format!("value is out of range for '{self}' relocation: '{value}'"),
                );
            }
            Self::ImmI16 if !(i16::MAX as i32..=i16::MIN as i32).contains(&(value as i32)) => {
                ctx.report_error(
                    node,
                    format!("value is out of range for '{self}' relocation: '{value}'"),
                );
            }
            Self::ImmU16 if !(0..=u16::MIN as u32).contains(&(value)) => {
                ctx.report_error(
                    node,
                    format!("value is out of range for '{self}' relocation: '{value}'"),
                );
            }
            Self::ImmH16 if value & 0xFFFF != 0 => {
                ctx.report_error(
                    node,
                    format!("value is out of range for '{self}' relocation: '{value}'"),
                );
            }
            Self::U8 if value & !0xFF != 0 => {
                ctx.report_error(
                    node,
                    format!("value is out of range for '{self}' relocation: '{value}'"),
                );
            }
            Self::U16 if value & !0xFF != 0 => {
                ctx.report_error(
                    node,
                    format!("value is out of range for '{self}' relocation: '{value}'"),
                );
            }
            _ => {}
        }
        self.generate(value)
    }

    pub fn checked_signed<'a>(&self, ctx: &mut Context<'a>, node: NodeRef<'a>, value: i32) -> u32 {
        match self {
            Self::BranchI16 if !(i16::MAX as i32..=i16::MIN as i32).contains(&(value >> 2)) => {
                ctx.report_error(
                    node,
                    format!("value is out of range for '{self}' relocation: '{value}'"),
                );
            }
            Self::ImmI16 if !(i16::MAX as i32..=i16::MIN as i32).contains(&value) => {
                ctx.report_error(
                    node,
                    format!("value is out of range for '{self}' relocation: '{value}'"),
                );
            }
            Self::ImmU16 if !(0..=u16::MIN as i32).contains(&value) => {
                ctx.report_error(
                    node,
                    format!("value is out of range for '{self}' relocation: '{value}'"),
                );
            }
            Self::ImmH16 if value & 0xFFFF != 0 => {
                ctx.report_error(
                    node,
                    format!("value is out of range for '{self}' relocation: '{value}'"),
                );
            }
            Self::U8 if value & !0xFF != 0 => {
                ctx.report_error(
                    node,
                    format!("value is out of range for '{self}' relocation: '{value}'"),
                );
            }
            Self::U16 if value & !0xFF != 0 => {
                ctx.report_error(
                    node,
                    format!("value is out of range for '{self}' relocation: '{value}'"),
                );
            }
            _ => {}
        }
        self.generate(value as u32)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum MipsRelocCalc {
    Absolute(SymbolIdx),
    Pcrel(SymbolIdx),
    Size(SymbolIdx),
    Align(SymbolIdx),
    Sub(SymbolIdx, SymbolIdx),
}

impl Reloc for MipsReloc {
    type Machine = MipsTranslationUnit;

    fn display(
        &self,
        f: &mut impl std::fmt::Write,
        trans: &TranslationUnit<Self::Machine>,
    ) -> std::fmt::Result {
        write!(f, "{}", self.pattern)?;
        if self.overflow {
            write!(f, "_overflow")?;
        }
        write!(f, "(")?;

        fn display_resolved(
            f: &mut impl std::fmt::Write,
            symbol_idx: SymbolIdx,
            trans: &TranslationUnit<MipsTranslationUnit>,
        ) -> std::fmt::Result {
            let sym = trans.get_symbol(symbol_idx);
            write!(f, "{}", trans.get_str(sym.name()).unwrap_or_default())?;
            if let Some(section) = sym.section {
                write!(
                    f,
                    "[{}",
                    trans.get_str(trans.get(section).name()).unwrap_or_default()
                )?;
                if sym.offset != 0 {
                    write!(f, "{:+x}", sym.offset)?;
                }
                write!(f, "]")?
            }
            write!(f, ")")
        }

        match self.calculation {
            MipsRelocCalc::Absolute(symbol_idx) => {
                write!(f, "abs(")?;
                display_resolved(f, symbol_idx, trans)?;
            }
            MipsRelocCalc::Pcrel(symbol_idx) => {
                write!(f, "pcrel(")?;
                display_resolved(f, symbol_idx, trans)?;
            }
            MipsRelocCalc::Size(symbol_idx) => {
                write!(f, "size(")?;
                display_resolved(f, symbol_idx, trans)?;
            }
            MipsRelocCalc::Align(symbol_idx) => {
                write!(f, "align(")?;
                display_resolved(f, symbol_idx, trans)?;
            }
            MipsRelocCalc::Sub(lhs, rhs) => {
                write!(f, "abs(")?;
                display_resolved(f, lhs, trans)?;
                write!(f, ")-abs(")?;
                display_resolved(f, rhs, trans)?;
            }
        }
        if self.offset != 0 {
            write!(f, "{:+}", self.offset)?;
        }
        write!(f, ")")?;

        Ok(())
    }
}
