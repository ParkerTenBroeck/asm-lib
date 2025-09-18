use assembler::{
    Assembler, Context, LangCtx, LogEntry, Node, NodeRef, expression::args::RegArg,
    simple::SimpleAssemblyLanguageBase,
};

use crate::{
    MipsAssembler,
    args::{Immediate, ImmediateI16, ImmediateU16, IndexedCoerced, ShiftConstant},
    indexed::MemoryIndex,
    label::{LabelExpr, LabelExprType, RelocPattern},
    opcodes::*,
    reg::Register,
    trans::{MipsReloc, MipsRelocCalc, MipsRelocPattern},
};

pub enum InstructionKind {
    IdxSaveMem,
    IdxLoadMem,
    Branch,
    Jump,
    Lui,
    ArithSigned,
    ArithUnsigned,
    La,
}

impl std::fmt::Display for InstructionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IdxSaveMem => write!(f, "save"),
            Self::IdxLoadMem => write!(f, "load"),
            Self::Branch => write!(f, "brance"),
            Self::Jump => write!(f, "jump"),
            Self::Lui => write!(f, "lui"),
            Self::ArithSigned => write!(f, "arith_signed"),
            Self::ArithUnsigned => write!(f, "arith_signed"),
            Self::La => write!(f, "la"),
        }
    }
}

impl InstructionKind {
    pub fn reloc<'a>(
        &self,
        ctx: Assembler<'a, '_, MipsAssembler<'a>>,
        node: NodeRef<'a>,
        label: LabelExpr<'a>,
    ) -> Option<MipsReloc> {
        let pattern = self.pattern(ctx.context, node, label.pattern)?;
        let calculation = self.calculation(ctx, node, label.ty)?;
        Some(MipsReloc {
            pattern,
            calculation,
            offset: label.offset,
            overflow: matches!(label.pattern, Some(RelocPattern::High | RelocPattern::Low))
                | matches!(pattern, MipsRelocPattern::ImmHighU16),
        })
    }
    pub fn pattern<'a>(
        &self,
        ctx: &mut Context<'a>,
        node: NodeRef<'a>,
        reloc: Option<RelocPattern>,
    ) -> Option<MipsRelocPattern> {
        Some(match (self, reloc) {
            (InstructionKind::IdxSaveMem, None) => MipsRelocPattern::ImmI16,
            (InstructionKind::IdxLoadMem, None) => MipsRelocPattern::ImmI16,
            (InstructionKind::Branch, None) => MipsRelocPattern::BranchI16,
            (InstructionKind::Jump, None) => MipsRelocPattern::JumpU26,
            (InstructionKind::Lui, None | Some(RelocPattern::High)) => MipsRelocPattern::ImmHighU16,
            (InstructionKind::Lui, Some(RelocPattern::Low)) => MipsRelocPattern::ImmU16,
            (InstructionKind::ArithSigned, None | Some(RelocPattern::Low)) => {
                MipsRelocPattern::ImmI16
            }
            (InstructionKind::ArithSigned, Some(RelocPattern::High)) => {
                MipsRelocPattern::ImmHighU16
            }
            (InstructionKind::ArithUnsigned, None | Some(RelocPattern::Low)) => {
                MipsRelocPattern::ImmU16
            }
            (InstructionKind::ArithUnsigned, Some(RelocPattern::High)) => {
                MipsRelocPattern::ImmHighU16
            }
            (InstructionKind::La, None) => None?,

            (_, Some(reloc)) => {
                ctx.report_error(
                    node,
                    format!("Cannot use '{reloc}' pattern with '{self}' class instructions"),
                );
                None?
            }
        })
    }
    pub fn calculation<'a>(
        &self,
        mut ctx: Assembler<'a, '_, MipsAssembler<'a>>,
        node: NodeRef<'a>,
        reloc: LabelExprType<'a>,
    ) -> Option<MipsRelocCalc> {
        let pcrel = matches!(self, InstructionKind::Branch);
        let reloc = reloc.reloc_type(&mut ctx, pcrel)?;
        Some(match (self, reloc) {
            (Self::Branch, MipsRelocCalc::Absolute(_)) => {
                ctx.context.report_warning(
                    node,
                    format!("absolute address used with '{self}' class instruction"),
                );
                reloc
            }
            (Self::Branch, _) => reloc,
            (Self::Jump, MipsRelocCalc::Absolute(_)) => reloc,
            (Self::Jump, _) => {
                ctx.context.report_warning(
                    node,
                    format!("non absolute address used with '{self}' class instruction"),
                );
                reloc
            }
            (
                Self::La
                | Self::ArithSigned
                | Self::ArithUnsigned
                | Self::Lui
                | Self::IdxLoadMem
                | Self::IdxSaveMem,
                _,
            ) => reloc,
        })
    }
}

impl<'a> MipsAssembler<'a> {
    pub fn instruction(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        ins: u32,
    ) {
        self.add_data(ctx, &ins.to_be_bytes(), 4, node);
    }

    pub fn instruction_reloc(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        ins: u32,
        reloc: MipsReloc,
    ) {
        self.current_section_mut(ctx, node)
            .reloc(reloc, Some(ctx.context.node_to_owned(node)));

        self.add_data(ctx, &ins.to_be_bytes(), 4, node);
    }
    pub fn r_type_rd_rs_rt(
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
    pub fn r_type_rd_rs(
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

    pub fn r_type_rs_rt(
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

    pub fn r_type_rd(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        opcode: Opcodes,
    ) {
        let Node(RegArg(rd), node) = ctx.eval(self).coerced(node);

        self.instruction(ctx, node, opcode as u32 + rd.unwrap_or_default().rd());
    }

    pub fn r_type_rs(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        opcode: Opcodes,
    ) {
        let Node(RegArg(rs), node) = ctx.eval(self).coerced(node);

        self.instruction(ctx, node, opcode as u32 + rs.unwrap_or_default().rs());
    }

    pub fn r_type_rd_rs_const(
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

    pub fn i_type_rt_rs_sign(
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

    pub fn i_type_rt_rs_unsign(
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

    pub fn i_type(
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

    pub fn i_type_rt(
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

    pub fn i_type_rt_rs(
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

    pub fn i_type_rt_idx(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        node: NodeRef<'a>,
        kind: InstructionKind,
        opcode: Opcodes,
    ) {
        let Node((RegArg(rt), IndexedCoerced(indexed)), node) = ctx.eval(self).coerced(node);
        let rt = rt.unwrap_or_default();
        let (rs, imm) = match indexed {
            MemoryIndex::LabelRegisterOffset(r, l) => (r, Immediate::Label(l)),
            MemoryIndex::RegisterOffset(r, i) => (r, Immediate::SignedConstant(i)),
        };
        self.with_immediate(ctx, node, kind, imm, rs, rt, opcode);
    }

    pub fn j_type(
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

    pub fn no_args(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        instruction: u32,
    ) {
        let Node((), node) = ctx.eval(self).coerced(node);
        self.instruction(ctx, node, instruction);
    }

    #[allow(clippy::too_many_arguments)]
    pub fn with_immediate(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        kind: InstructionKind,
        immediate: Immediate<'a>,
        rs: Register,
        rt: Register,
        instruction: Opcodes,
    ) {
        use crate::trans::MipsRelocPattern as MRP;
        match kind {
            InstructionKind::La => match immediate {
                Immediate::SignedConstant(signed) => {
                    let slice: &[u32] = match signed {
                        -0x8000..=0x7FFF => &[Opcodes::Addi as u32
                            + MRP::ImmI16.generate(signed as u32)
                            + rt.rt()],
                        _ if signed & 0xFFFF == 0 => &[Opcodes::Lui as u32
                            + MRP::ImmHighU16.generate(signed as u32)
                            + rt.rt()],
                        _ => &[
                            Opcodes::Lui as u32 + MRP::ImmHighU16.generate(signed as u32) + rt.rt(),
                            Opcodes::Ori as u32 + MRP::ImmU16.generate(signed as u32) + rt.rt(),
                        ],
                    };
                    for ins in slice {
                        self.instruction(ctx, node, *ins);
                    }
                }
                Immediate::UnsignedConstant(unsigned) => {
                    let slice: &[u32] = match unsigned {
                        0..=0xFFFF => {
                            &[Opcodes::Ori as u32 + MRP::ImmU16.generate(unsigned) + rt.rt()]
                        }
                        0x10000..=0xFFFF_FFFF if unsigned & 0xFFFF == 0 => {
                            &[Opcodes::Lui as u32 + MRP::ImmHighU16.generate(unsigned) + rt.rt()]
                        }
                        0x10000..=0xFFFF_FFFF => &[
                            Opcodes::Lui as u32 + MRP::ImmHighU16.generate(unsigned) + rt.rt(),
                            Opcodes::Ori as u32 + MRP::ImmU16.generate(unsigned) + rt.rt(),
                        ],
                    };
                    for ins in slice {
                        self.instruction(ctx, node, *ins);
                    }
                }
                Immediate::Label(label) => {
                    let Some(calculation) = kind.calculation(ctx.asm(self), node, label.ty) else {
                        return;
                    };
                    _ = kind.pattern(ctx.context, node, label.pattern);
                    self.instruction_reloc(
                        ctx,
                        node,
                        Opcodes::Lui as u32 + rt.rt(),
                        MipsReloc::without_overflow(MRP::ImmHighU16, calculation, label.offset),
                    );
                    self.instruction_reloc(
                        ctx,
                        node,
                        Opcodes::Ori as u32 + rt.rt(),
                        MipsReloc::with_overflow(MRP::ImmU16, calculation, label.offset),
                    );
                }
            },
            InstructionKind::IdxSaveMem | InstructionKind::IdxLoadMem => match immediate {
                Immediate::SignedConstant(value) => {
                    if MRP::ImmI16.signed_fits(value) {
                        self.instruction(
                            ctx,
                            node,
                            instruction as u32
                                + rs.rs()
                                + rt.rt()
                                + MRP::ImmI16.generate(value as u32),
                        );
                    } else if rs == Register::ZERO {
                        self.instruction(
                            ctx,
                            node,
                            Opcodes::Lui as u32 + rt.rt() + MRP::ImmHighI16.generate(value as u32),
                        );
                        self.instruction(
                            ctx,
                            node,
                            instruction as u32
                                + rt.rs()
                                + rt.rt()
                                + MRP::ImmI16.generate(value as u32),
                        );
                    } else if rs != rt || rs != Register::ASM_RESERVE {
                        let tmp = if rs != Register::ASM_RESERVE {
                            Register::ASM_RESERVE
                        } else {
                            rt
                        };
                        self.instruction(
                            ctx,
                            node,
                            Opcodes::Lui as u32
                                + tmp.rt()
                                + MipsRelocPattern::ImmHighI16.generate(value as u32),
                        );
                        self.instruction(
                            ctx,
                            node,
                            Opcodes::Add as u32 + tmp.rd() + rs.rs() + tmp.rt(),
                        );
                        self.instruction(
                            ctx,
                            node,
                            instruction as u32
                                + rt.rt()
                                + tmp.rs()
                                + MipsRelocPattern::ImmI16.generate(value as u32),
                        );
                    } else {
                        ctx.context.report(
                            LogEntry::new()
                                .error(node, "cannot generate assembly")
                                .hint_locless("avoid using at/x1 it is reserved for the assembler"),
                        );
                    }
                }
                Immediate::UnsignedConstant(value) => {
                    if MRP::ImmI16.unsigned_fits(value) {
                        self.instruction(
                            ctx,
                            node,
                            instruction as u32 + rs.rs() + rt.rt() + MRP::ImmI16.generate(value),
                        );
                    } else if rs == Register::ZERO {
                        self.instruction(
                            ctx,
                            node,
                            Opcodes::Lui as u32 + rt.rt() + MRP::ImmHighI16.generate(value),
                        );
                        self.instruction(
                            ctx,
                            node,
                            instruction as u32 + rt.rs() + rt.rt() + MRP::ImmI16.generate(value),
                        );
                    } else if rs != rt || rs != Register::ASM_RESERVE {
                        let tmp = if rs != Register::ASM_RESERVE {
                            Register::ASM_RESERVE
                        } else {
                            rt
                        };
                        self.instruction(
                            ctx,
                            node,
                            Opcodes::Lui as u32
                                + tmp.rt()
                                + MipsRelocPattern::ImmHighI16.generate(value),
                        );
                        self.instruction(
                            ctx,
                            node,
                            Opcodes::Add as u32 + tmp.rd() + rs.rs() + tmp.rt(),
                        );
                        self.instruction(
                            ctx,
                            node,
                            instruction as u32
                                + rt.rt()
                                + tmp.rs()
                                + MipsRelocPattern::ImmI16.generate(value),
                        );
                    } else {
                        ctx.context.report(
                            LogEntry::new()
                                .error(node, "cannot generate assembly")
                                .hint_locless("avoid using at/x1 it is reserved for the assembler"),
                        );
                    }
                }
                Immediate::Label(label) => {
                    let Some(reloc) = kind.reloc(ctx.asm(self), node, label) else {
                        return;
                    };
                    if rs == Register::ZERO {
                        self.instruction_reloc(
                            ctx,
                            node,
                            Opcodes::Lui as u32 + rt.rt(),
                            reloc
                                .with_pattern(MipsRelocPattern::ImmHighI16)
                                .overflow(true),
                        );
                        self.instruction_reloc(
                            ctx,
                            node,
                            instruction as u32 + rt.rs() + rt.rt(),
                            reloc.with_pattern(MipsRelocPattern::ImmI16).overflow(true),
                        );
                    } else if rs != rt || rs != Register::ASM_RESERVE {
                        let tmp = if rs != Register::ASM_RESERVE {
                            Register::ASM_RESERVE
                        } else {
                            rt
                        };
                        self.instruction_reloc(
                            ctx,
                            node,
                            Opcodes::Lui as u32 + tmp.rt(),
                            reloc
                                .with_pattern(MipsRelocPattern::ImmHighI16)
                                .overflow(true),
                        );
                        self.instruction(
                            ctx,
                            node,
                            Opcodes::Add as u32 + tmp.rd() + rs.rs() + tmp.rt(),
                        );
                        self.instruction_reloc(
                            ctx,
                            node,
                            instruction as u32 + rt.rt() + tmp.rs(),
                            reloc.with_pattern(MipsRelocPattern::ImmI16).overflow(true),
                        );
                    } else {
                        ctx.context.report(
                            LogEntry::new()
                                .error(node, "cannot generate assembly")
                                .hint_locless("avoid using at/x1 it is reserved for the assembler"),
                        );
                    }
                }
            },
            InstructionKind::Branch => match immediate {
                Immediate::SignedConstant(signed) => {
                    let imm = MRP::BranchI16.checked_signed(ctx.context, node, signed);
                    self.instruction(ctx, node, instruction as u32 + rs.rs() + rt.rt() + imm);
                }
                Immediate::UnsignedConstant(unsigned) => {
                    let imm = MRP::BranchI16.checked_unsigned(ctx.context, node, unsigned);
                    self.instruction(ctx, node, instruction as u32 + rs.rs() + rt.rt() + imm);
                }
                Immediate::Label(label) => {
                    let Some(reloc) = kind.reloc(ctx.asm(self), node, label) else {
                        return;
                    };
                    self.instruction_reloc(
                        ctx,
                        node,
                        instruction as u32 + rs.rs() + rt.rt(),
                        reloc,
                    );
                }
            },
            InstructionKind::Lui => match immediate {
                Immediate::SignedConstant(value) => {
                    let imm = MipsRelocPattern::ImmI16.checked_signed(ctx.context, node, value);
                    self.instruction(ctx, node, instruction as u32 | rs.rs() | rt.rt() | imm);
                }
                Immediate::UnsignedConstant(value) => {
                    let imm = MipsRelocPattern::ImmU16.checked_unsigned(ctx.context, node, value);
                    self.instruction(ctx, node, instruction as u32 | rs.rs() | rt.rt() | imm);
                }
                Immediate::Label(label) => {
                    let Some(reloc) = kind.reloc(ctx.asm(self), node, label) else {
                        return;
                    };
                    self.instruction_reloc(
                        ctx,
                        node,
                        instruction as u32 + rs.rs() + rt.rt(),
                        reloc,
                    );
                }
            },
            InstructionKind::ArithSigned => match immediate {
                Immediate::SignedConstant(value) => {
                    let imm = MipsRelocPattern::ImmI16.checked_signed(ctx.context, node, value);
                    self.instruction(ctx, node, instruction as u32 | rs.rs() | rt.rt() | imm);
                }
                Immediate::UnsignedConstant(value) => {
                    let imm = MipsRelocPattern::ImmI16.checked_unsigned(ctx.context, node, value);
                    self.instruction(ctx, node, instruction as u32 | rs.rs() | rt.rt() | imm);
                }
                Immediate::Label(label) => {
                    let Some(reloc) = kind.reloc(ctx.asm(self), node, label) else {
                        return;
                    };
                    self.instruction_reloc(
                        ctx,
                        node,
                        instruction as u32 + rs.rs() + rt.rt(),
                        reloc,
                    );
                }
            },
            InstructionKind::ArithUnsigned => match immediate {
                Immediate::SignedConstant(value) => {
                    let imm = MipsRelocPattern::ImmU16.checked_signed(ctx.context, node, value);
                    self.instruction(ctx, node, instruction as u32 | rs.rs() | rt.rt() | imm);
                }
                Immediate::UnsignedConstant(value) => {
                    let imm = MipsRelocPattern::ImmU16.checked_unsigned(ctx.context, node, value);
                    self.instruction(ctx, node, instruction as u32 | rs.rs() | rt.rt() | imm);
                }
                Immediate::Label(label) => {
                    let Some(reloc) = kind.reloc(ctx.asm(self), node, label) else {
                        return;
                    };
                    self.instruction_reloc(
                        ctx,
                        node,
                        instruction as u32 + rs.rs() + rt.rt(),
                        reloc,
                    );
                }
            },
            InstructionKind::Jump => match immediate {
                Immediate::Label(label) => {
                    let Some(reloc) = kind.reloc(ctx.asm(self), node, label) else {
                        return;
                    };
                    self.instruction_reloc(
                        ctx,
                        node,
                        instruction as u32 + rs.rs() + rt.rt(),
                        reloc,
                    );
                }
                Immediate::SignedConstant(v) => {
                    if v.is_negative() {
                        ctx.context
                            .report_warning(node, "negative constant used in jump instruction");
                    }
                    let imm = MRP::JumpU26.checked_signed(ctx.context, node, v);
                    self.instruction(ctx, node, instruction as u32 + rs.rs() + rt.rt() + imm);
                }
                Immediate::UnsignedConstant(v) => {
                    let imm = MRP::JumpU26.checked_unsigned(ctx.context, node, v);
                    self.instruction(ctx, node, instruction as u32 + rs.rs() + rt.rt() + imm);
                }
            },
        }
    }
}
