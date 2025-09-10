use assembler::{
    LangCtx, Node, NodeRef,
    expression::args::{IndexedArg, RegArg},
    simple::{SimpleAssemblyLanguage, SimpleAssemblyLanguageBase},
};

use crate::{
    MipsAssembler,
    args::{Immediate, ImmediateI16, ImmediateU16, ShiftConstant},
    indexed::MemoryIndex,
    lang::InstructionKind,
    opcodes::*,
    reg::Register,
    trans::MipsReloc,
};

impl<'a> MipsAssembler<'a> {
    pub(crate) fn instruction(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        ins: u32,
    ) {
        self.current_section_mut(ctx, node).data(
            &ins.to_be_bytes(),
            4,
            Some(ctx.context.node_to_owned(node)),
        );
    }
    pub(crate) fn r_type_rd_rs_rt(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        opcode: Opcodes,
    ) {
        let Node((RegArg(rd), RegArg(rs), RegArg(rt)), node) = ctx.eval(self).coerced(node);

        self.instruction(
            ctx,
            node,
            opcode as u32
                + rd.unwrap_or_default().rd()
                + rs.unwrap_or_default().rs()
                + rt.unwrap_or_default().rt(),
        );
    }
    pub(crate) fn r_type_rd_rs(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        opcode: Opcodes,
    ) {
        let Node((RegArg(rd), RegArg(rs)), node) = ctx.eval(self).coerced(node);

        self.instruction(
            ctx,
            node,
            opcode as u32 + rd.unwrap_or_default().rd() + rs.unwrap_or_default().rs(),
        );
    }

    pub(crate) fn r_type_rs_rt(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        opcode: Opcodes,
    ) {
        let Node((RegArg(rs), RegArg(rt)), node) = ctx.eval(self).coerced(node);

        self.instruction(
            ctx,
            node,
            opcode as u32 + rs.unwrap_or_default().rs() + rt.unwrap_or_default().rt(),
        );
    }

    pub(crate) fn r_type_rd(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        opcode: Opcodes,
    ) {
        let Node(RegArg(rd), node) = ctx.eval(self).coerced(node);

        self.instruction(ctx, node, opcode as u32 + rd.unwrap_or_default().rd());
    }

    pub(crate) fn r_type_rs(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        opcode: Opcodes,
    ) {
        let Node(RegArg(rs), node) = ctx.eval(self).coerced(node);

        self.instruction(ctx, node, opcode as u32 + rs.unwrap_or_default().rs());
    }

    pub(crate) fn r_type_rd_rs_const(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        opcode: Opcodes,
    ) {
        let Node((RegArg(rd), RegArg(rs), shift), node): Node<(_, _, ShiftConstant)> =
            ctx.eval(self).coerced(node);

        self.instruction(
            ctx,
            node,
            opcode as u32 + rd.unwrap_or_default().rs() + rs.unwrap_or_default().rt() + rt(shift.0),
        );
    }

    pub(crate) fn i_type_rt_rs_sign(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        opcode: Opcodes,
    ) {
        let Node((RegArg(rt), RegArg(rs), imm), node) = ctx.eval(self).coerced(node);
        let rt = rt.unwrap_or_default();
        let rs = rs.unwrap_or_default();
        match imm {
            ImmediateI16::Constant(v) => self.instruction(
                ctx,
                node,
                opcode as u32 + rs.rs() + rt.rt() + imm_16(v as u32),
            ),
            ImmediateI16::Label(l) => {
                self.with_immediate(
                    ctx,
                    node,
                    InstructionKind::ArithSigned,
                    Immediate::Label(l),
                    rs,
                    rt,
                    opcode,
                );
            }
        };
    }

    pub(crate) fn i_type_rt_rs_unsign(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        opcode: Opcodes,
    ) {
        let Node((RegArg(rt), RegArg(rs), imm), node) = ctx.eval(self).coerced(node);
        let rt = rt.unwrap_or_default();
        let rs = rs.unwrap_or_default();
        match imm {
            ImmediateU16::Constant(v) => {
                self.instruction(
                    ctx,
                    node,
                    opcode as u32 + rs.rs() + rt.rt() + imm_16(v as u32),
                );
            }
            ImmediateU16::Label(l) => self.with_immediate(
                ctx,
                node,
                InstructionKind::ArithUnsigned,
                Immediate::Label(l),
                rs,
                rt,
                opcode,
            ),
        };
    }

    pub(crate) fn i_type(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        kind: InstructionKind,
        opcode: Opcodes,
    ) {
        let Node(immediate, node) = ctx.eval(self).coerced(node);
        self.with_immediate(
            ctx,
            node,
            kind,
            immediate,
            Register::ZERO,
            Register::ZERO,
            opcode,
        );
    }

    pub(crate) fn i_type_rt(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        kind: InstructionKind,
        opcode: Opcodes,
    ) {
        let Node((RegArg(rt), immediate), node) = ctx.eval(self).coerced(node);
        let rt = rt.unwrap_or_default();
        self.with_immediate(ctx, node, kind, immediate, Register::ZERO, rt, opcode);
    }

    pub(crate) fn i_type_rt_rs(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        kind: InstructionKind,
        opcode: Opcodes,
    ) {
        let Node((RegArg(rt), RegArg(rs), immediate), node) = ctx.eval(self).coerced(node);
        let rt = rt.unwrap_or_default();
        let rs = rs.unwrap_or_default();
        self.with_immediate(ctx, node, kind, immediate, rs, rt, opcode);
    }

    pub(crate) fn i_type_rt_idx(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        node: NodeRef<'a>,
        kind: InstructionKind,
        opcode: Opcodes,
    ) {
        let Node((RegArg(rt), IndexedArg(indexed)), node) = ctx.eval(self).coerced(node);
        let rt = rt.unwrap_or_default();
        let indexed = indexed.unwrap_or_default();
        let (rs, imm) = match indexed {
            MemoryIndex::LabelRegisterOffset(r, l) => (r, Immediate::Label(l)),
            MemoryIndex::RegisterOffset(r, i) => (r, Immediate::SignedConstant(i)),
        };
        self.with_immediate(ctx, node, kind, imm, rs, rt, opcode);
    }

    pub(crate) fn j_type(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        opcode: Opcodes,
    ) {
        let Node(immediate, node) = ctx.eval(self).coerced(node);
        self.with_immediate(
            ctx,
            node,
            InstructionKind::Jump,
            immediate,
            Register::ZERO,
            Register::ZERO,
            opcode,
        );
    }

    pub(crate) fn no_args(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        instruction: u32,
    ) {
        let Node((), node) = ctx.eval(self).coerced(node);
        self.instruction(ctx, node, instruction);
    }

    pub(crate) fn with_immediate(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        kind: InstructionKind,
        immediate: Immediate<'a>,
        rs: Register,
        rt: Register,
        instruction: Opcodes,
    ) {
        use crate::trans::MipsRelocCalc as MRC;
        use crate::trans::MipsRelocPattern as MRP;

        use crate::label::LabelExprType as LET;
        match kind {
            InstructionKind::La => match immediate {
                Immediate::SignedConstant(signed)
                    if i16::MIN as i32 <= signed && signed <= i16::MAX as i32 =>
                {
                    self.instruction(
                        ctx,
                        node,
                        Opcodes::Addi as u32 + imm_16(signed as u32) + rt.rt(),
                    );
                }
                Immediate::SignedConstant(signed) if signed <= u16::MAX as i32 => {
                    self.instruction(
                        ctx,
                        node,
                        Opcodes::Ori as u32 + imm_16(signed as u32) + rt.rt(),
                    );
                }
                Immediate::SignedConstant(signed) => {
                    self.instruction(
                        ctx,
                        node,
                        Opcodes::Lui as u32 + imm_16(signed as u32 >> 16) + rt.rt(),
                    );
                    if signed as u16 != 0 {
                        self.instruction(
                            ctx,
                            node,
                            Opcodes::Ori as u32 + imm_16(signed as u32) + rt.rt(),
                        );
                    }
                }
                Immediate::UnsignedConstant(unsigned) if unsigned <= u16::MAX as u32 => {
                    self.instruction(ctx, node, Opcodes::Ori as u32 + imm_16(unsigned) + rt.rt());
                }
                Immediate::UnsignedConstant(unsigned) => {
                    self.instruction(
                        ctx,
                        node,
                        Opcodes::Lui as u32 + imm_16(unsigned >> 16) + rt.rt(),
                    );
                    if unsigned as u16 != 0 {
                        self.instruction(
                            ctx,
                            node,
                            Opcodes::Ori as u32 + imm_16(unsigned) + rt.rt(),
                        );
                    }
                }
                Immediate::Label(label_expr) => {
                    if let Some(calculation) = label_expr.ty.reloc_type(ctx.asm(self), false) {
                        if label_expr.pattern.is_some() {
                            ctx.context
                                .report_error(node, "no pattern can be set for la or li");
                        }
                        self.current_section_mut(ctx, node).reloc(
                            MipsReloc {
                                pattern: MRP::ImmH16,
                                calculation,
                                offset: label_expr.offset,
                                overflow: false,
                            },
                            Some(ctx.context.node_to_owned(node)),
                        );
                        self.instruction(ctx, node, Opcodes::Lui as u32 + rt.rt());
                        self.current_section_mut(ctx, node).reloc(
                            MipsReloc {
                                pattern: MRP::ImmU16,
                                calculation,
                                offset: label_expr.offset,
                                overflow: false,
                            },
                            Some(ctx.context.node_to_owned(node)),
                        );
                        self.instruction(ctx, node, Opcodes::Ori as u32 + rt.rt());
                    } else {
                        self.instruction(ctx, node, Opcodes::Lui as u32 + rt.rt());
                        self.instruction(ctx, node, Opcodes::Ori as u32 + rt.rt());
                    }
                }
            },
            InstructionKind::IdxSaveMem | InstructionKind::IdxLoadMem => {
                self.instruction(ctx, node, instruction as u32 + rs.rs() + rt.rt());
            }
            InstructionKind::Branch => {
                match immediate {
                    Immediate::SignedConstant(_) => todo!(),
                    Immediate::UnsignedConstant(_) => todo!(),
                    Immediate::Label(label_expr) => todo!(),
                }
                self.instruction(ctx, node, instruction as u32 + rs.rs() + rt.rt());
            }
            InstructionKind::Lui => {
                self.instruction(ctx, node, instruction as u32 + rs.rs() + rt.rt());
            }
            InstructionKind::ArithSigned => {
                self.instruction(ctx, node, instruction as u32 + rs.rs() + rt.rt());
            }
            InstructionKind::ArithUnsigned => {
                self.instruction(ctx, node, instruction as u32 + rs.rs() + rt.rt());
            }
            InstructionKind::Jump => match immediate {
                Immediate::Label(l) => {
                    let calculation = match l.ty {
                        LET::Empty => return Default::default(),
                        LET::Unspecified(label) | LET::Absolute(label) => MRC::Absolute(
                            self.state_mut().trans.resolve_or_make_symbol(label.ident),
                        ),
                        LET::PcRel(label) => {
                            ctx.context.report_warning(node, format!("jump instruction target '{l}' defined as pcrel when jump requires absolute"));
                            MRC::Pcrel(self.state_mut().trans.resolve_or_make_symbol(label.ident))
                        }
                        LET::Size(label) => {
                            ctx.context.report_warning(node, format!("jump instruction target '{l}' defined as size when jump requires absolute"));
                            MRC::Size(self.state_mut().trans.resolve_or_make_symbol(label.ident))
                        }
                        LET::Align(label) => {
                            ctx.context.report_warning(node, format!("jump instruction target '{l}' defined as align when jump requires absolute"));
                            MRC::Align(self.state_mut().trans.resolve_or_make_symbol(label.ident))
                        }
                        LET::Sub(lhs, rhs) => {
                            ctx.context.report_warning(node, format!("jump instruction target '{l}' defined as sub when jump requires absolute"));
                            MRC::Sub(
                                self.state_mut().trans.resolve_or_make_symbol(lhs.ident),
                                self.state_mut().trans.resolve_or_make_symbol(rhs.ident),
                            )
                        }
                    };
                    self.current_section_mut(ctx, node).reloc(
                        MipsReloc {
                            pattern: MRP::JumpU26,
                            calculation,
                            offset: l.offset,
                            overflow: false,
                        },
                        Some(ctx.context.node_to_owned(node)),
                    );
                    self.instruction(ctx, node, instruction as u32 + rs.rs() + rt.rt());
                }
                Immediate::SignedConstant(v) => {
                    if v.is_negative() {
                        ctx.context
                            .report_warning(node, "negative constant used in jump instruction");
                    }
                    self.instruction(
                        ctx,
                        node,
                        instruction as u32 + rs.rs() + rt.rt() + imm_26(v as u32 >> 2),
                    );
                }
                Immediate::UnsignedConstant(v) if v > 0x3FFFFFF => {
                    ctx.context
                        .report_warning(node, "constant cannot fit in 26 bits");
                    self.instruction(ctx, node, instruction as u32);
                }
                Immediate::UnsignedConstant(v) => {
                    self.instruction(
                        ctx,
                        node,
                        instruction as u32 + rs.rs() + rt.rt() + imm_26(v >> 2),
                    );
                }
            },
        }
    }
}
