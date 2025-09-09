use assembler::simple::trans::{TranslationUnitMachine, reloc::Reloc, section::Section};

use crate::reg::Register;

pub struct MipsTranslationUnit;

impl TranslationUnitMachine for MipsTranslationUnit {
    type Reloc = MipsReloc;
    type PtrSizeType = u32;

    fn fmt_section_disassembly(
        section: &Section<Self>,
        trans: &assembler::simple::trans::TranslationUnit<Self>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let mut offset = 0;
        let chunk_size = 4;
        let ptr_size = std::mem::size_of::<Self::PtrSizeType>() * 2;

        for chunk in section.data().slice().chunks(chunk_size) {
            write!(f, "{offset:0>ptr_size$x}: ")?;

            for byte in chunk {
                write!(f, "{byte:0>2x} ")?;
            }
            for _ in chunk.len()..chunk_size {
                write!(f, "   ")?;
            }

            write!(f, "   ")?;

            let instruction =
                u32::from_be_bytes(std::array::from_fn(|i| chunk.get(i).copied().unwrap_or(0)));

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

            writeln!(f)?;
            offset += chunk.len();
        }

        writeln!(f)?;
        section.relocations().fmt(f, trans)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct MipsReloc {}

impl Reloc for MipsReloc {}
