use assembler::{
    Node,
    expression::args::{IndexedArg, RegArg},
};

use crate::{
    MipsAssembler, args::Immediate, indexed::MemoryIndex, lang::InstructionKind, opcodes::Opcodes,
    reg::Register,
};

impl<'a> MipsAssembler<'a> {
    pub(crate) fn assemble_mnemonic_impl(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        mnemonic: &'a str,
        node: assembler::NodeRef<'a>,
    ) {
        match mnemonic {
            "add" => self.r_type_rd_rs_rt(ctx, node, Opcodes::Add),
            "addu" => self.r_type_rd_rs_rt(ctx, node, Opcodes::Addu),
            "sub" => self.r_type_rd_rs_rt(ctx, node, Opcodes::Sub),
            "subu" => self.r_type_rd_rs_rt(ctx, node, Opcodes::Subu),
            "and" => self.r_type_rd_rs_rt(ctx, node, Opcodes::And),
            "or" => self.r_type_rd_rs_rt(ctx, node, Opcodes::Or),
            "nor" => self.r_type_rd_rs_rt(ctx, node, Opcodes::Nor),
            "xor" => self.r_type_rd_rs_rt(ctx, node, Opcodes::Xor),

            "sllv" => self.r_type_rd_rs_rt(ctx, node, Opcodes::Sllv),
            "sll" => self.r_type_rd_rs_const(ctx, node, Opcodes::Sll),
            "srav" => self.r_type_rd_rs_rt(ctx, node, Opcodes::Srav),
            "sra" => self.r_type_rd_rs_const(ctx, node, Opcodes::Sra),
            "srlv" => self.r_type_rd_rs_rt(ctx, node, Opcodes::Srlv),
            "srl" => self.r_type_rd_rs_const(ctx, node, Opcodes::Srl),

            "slt" => self.r_type_rd_rs_rt(ctx, node, Opcodes::Slt),
            "sltu" => self.r_type_rd_rs_rt(ctx, node, Opcodes::Sltu),

            "mfhi" => self.r_type_rd(ctx, node, Opcodes::Mfhi),
            "mflo" => self.r_type_rd(ctx, node, Opcodes::Mflo),
            "mthi" => self.r_type_rs(ctx, node, Opcodes::Mthi),
            "mtlo" => self.r_type_rs(ctx, node, Opcodes::Mtlo),

            "mult" => self.r_type_rs_rt(ctx, node, Opcodes::Mult),
            "multu" => self.r_type_rs_rt(ctx, node, Opcodes::Multu),
            "div" => self.r_type_rs_rt(ctx, node, Opcodes::Div),
            "divu" => self.r_type_rs_rt(ctx, node, Opcodes::Divu),

            "addi" => self.i_type_rt_rs_sign(ctx, node, Opcodes::Addi),
            "addiu" => self.i_type_rt_rs_sign(ctx, node, Opcodes::Addiu),
            "ori" => self.i_type_rt_rs_unsign(ctx, node, Opcodes::Ori),
            "xori" => self.i_type_rt_rs_unsign(ctx, node, Opcodes::Xori),
            "andi" => self.i_type_rt_rs_unsign(ctx, node, Opcodes::Andi),

            "slit" => self.i_type_rt_rs_sign(ctx, node, Opcodes::Slti),
            "sltiu" => self.i_type_rt_rs_unsign(ctx, node, Opcodes::Sltiu),

            "lb" => self.i_type_rt_idx(ctx, node, InstructionKind::IdxLoadMem, Opcodes::Lb),
            "lbu" => self.i_type_rt_idx(ctx, node, InstructionKind::IdxLoadMem, Opcodes::Lbu),
            "lh" => self.i_type_rt_idx(ctx, node, InstructionKind::IdxLoadMem, Opcodes::Lh),
            "lhu" => self.i_type_rt_idx(ctx, node, InstructionKind::IdxLoadMem, Opcodes::Lhu),
            "lw" => self.i_type_rt_idx(ctx, node, InstructionKind::IdxLoadMem, Opcodes::Lw),
            "lwl" => self.i_type_rt_idx(ctx, node, InstructionKind::IdxLoadMem, Opcodes::Lwl),
            "lwr" => self.i_type_rt_idx(ctx, node, InstructionKind::IdxLoadMem, Opcodes::Lwr),

            "sb" => self.i_type_rt_idx(ctx, node, InstructionKind::IdxSaveMem, Opcodes::Sb),
            "sh" => self.i_type_rt_idx(ctx, node, InstructionKind::IdxSaveMem, Opcodes::Sh),
            "sw" => self.i_type_rt_idx(ctx, node, InstructionKind::IdxSaveMem, Opcodes::Sw),
            "swl" => self.i_type_rt_idx(ctx, node, InstructionKind::IdxSaveMem, Opcodes::Swl),
            "swr" => self.i_type_rt_idx(ctx, node, InstructionKind::IdxSaveMem, Opcodes::Swr),

            "j" => self.j_type(ctx, node, Opcodes::J),
            "jal" => self.j_type(ctx, node, Opcodes::Jal),
            "jalr" => self.r_type_rd_rs(ctx, node, Opcodes::Jalr),
            "jr" => self.r_type_rs(ctx, node, Opcodes::Jr),

            "beq" => self.i_type_rt_rs(ctx, node, InstructionKind::Branch, Opcodes::Beq),
            "bgez" => self.i_type_rt(ctx, node, InstructionKind::Branch, Opcodes::Bgez),
            "bgezal" => self.i_type_rt(ctx, node, InstructionKind::Branch, Opcodes::Bgezal),
            "bgtz" => self.i_type_rt(ctx, node, InstructionKind::Branch, Opcodes::Bgtz),
            "blez" => self.i_type_rt(ctx, node, InstructionKind::Branch, Opcodes::Blez),
            "bltz" => self.i_type_rt(ctx, node, InstructionKind::Branch, Opcodes::Bltz),
            "bltzal" => self.i_type_rt(ctx, node, InstructionKind::Branch, Opcodes::Bltzal),
            "bne" => self.i_type_rt_rs(ctx, node, InstructionKind::Branch, Opcodes::Bne),

            "lui" => self.i_type_rt(ctx, node, InstructionKind::Lui, Opcodes::Lui),

            "break" => self.no_args(ctx, node, Opcodes::Break as u32),
            "syscall" => self.no_args(ctx, node, Opcodes::Syscall as u32),

            // pseudo instructions
            "move" => self.r_type_rd_rs(ctx, node, Opcodes::Addu),

            "not" => self.r_type_rd_rs(ctx, node, Opcodes::Nor),

            "ret" => self.no_args(ctx, node, Opcodes::Jr as u32 + Register(31).rs()),

            "b" => self.i_type(ctx, node, InstructionKind::Branch, Opcodes::Bgez),
            "bal" => self.i_type(ctx, node, InstructionKind::Branch, Opcodes::Bgezal),
            "bnez" => self.i_type_rt(ctx, node, InstructionKind::Branch, Opcodes::Bne),

            "blt" | "ble" | "bgt" | "bge" => {
                let Node((RegArg(lhs), RegArg(rhs)), node) = ctx.eval(self).coerced(node);

                let lhs = lhs.unwrap_or_default();
                let rhs = rhs.unwrap_or_default();
                match mnemonic {
                    "blt" => self.instruction(
                        ctx,
                        node,
                        Opcodes::Slt as u32 + Register(1).rd() + lhs.rs() + rhs.rt(),
                    ),
                    "ble" => self.instruction(
                        ctx,
                        node,
                        Opcodes::Slt as u32 + Register(1).rd() + lhs.rt() + rhs.rs(),
                    ),
                    "bgt" => self.instruction(
                        ctx,
                        node,
                        Opcodes::Slt as u32 + Register(1).rd() + lhs.rs() + rhs.rt(),
                    ),
                    "bge" => self.instruction(
                        ctx,
                        node,
                        Opcodes::Slt as u32 + Register(1).rd() + lhs.rt() + rhs.rs(),
                    ),
                    _ => unreachable!(),
                }
                match mnemonic {
                    "blt" => self.instruction(ctx, node, Opcodes::Bne as u32 + Register(1).rs()),
                    "ble" => self.instruction(ctx, node, Opcodes::Beq as u32 + Register(1).rs()),
                    "bgt" => self.instruction(ctx, node, Opcodes::Bne as u32 + Register(1).rs()),
                    "bge" => self.instruction(ctx, node, Opcodes::Beq as u32 + Register(1).rs()),
                    _ => unreachable!(),
                }
            }

            "ulw" => {
                let Node((RegArg(rt), IndexedArg(indexed)), node) = ctx.eval(self).coerced(node);

                let rt = rt.unwrap_or_default();
                let (rs, immediate) = match indexed.unwrap_or_default() {
                    MemoryIndex::LabelRegisterOffset(r, l) => (r, Immediate::Label(l)),
                    MemoryIndex::RegisterOffset(r, i) => (r, Immediate::SignedConstant(i)),
                };

                self.with_immediate(
                    ctx,
                    node,
                    InstructionKind::IdxLoadMem,
                    immediate,
                    rs,
                    rt,
                    Opcodes::Swl,
                );
                self.with_immediate(
                    ctx,
                    node,
                    InstructionKind::IdxLoadMem,
                    immediate,
                    rs,
                    rt,
                    Opcodes::Swr,
                );
            }
            "usw" => {
                let Node((RegArg(rt), IndexedArg(indexed)), node) = ctx.eval(self).coerced(node);
                let rt = rt.unwrap_or_default();
                let (rs, immediate) = match indexed.unwrap_or_default() {
                    MemoryIndex::LabelRegisterOffset(r, l) => (r, Immediate::Label(l)),
                    MemoryIndex::RegisterOffset(r, i) => (r, Immediate::SignedConstant(i)),
                };

                self.with_immediate(
                    ctx,
                    node,
                    InstructionKind::IdxSaveMem,
                    immediate,
                    rs,
                    rt,
                    Opcodes::Lwl,
                );
                self.with_immediate(
                    ctx,
                    node,
                    InstructionKind::IdxSaveMem,
                    immediate,
                    rs,
                    rt,
                    Opcodes::Lwr,
                );
            }

            "la" | "li" => {
                let Node((RegArg(rt), immediate), node): Node<(_, Immediate)> =
                    ctx.eval(self).coerced(node);

                let rt = rt.unwrap_or_default();

                self.with_immediate(
                    ctx,
                    node,
                    InstructionKind::La,
                    immediate,
                    Register::ZERO,
                    rt,
                    Opcodes::Sll,
                );
            }

            _ => ctx.asm(self).unknown_mnemonic(mnemonic, node),
        }
    }
}
