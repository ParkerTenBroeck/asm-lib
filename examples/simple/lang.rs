use std::str::FromStr;

use assembler::{
    LangCtx, Node, NodeRef,
    expression::{
        Constant, EmptyCustomValue, ExprCtx, Value, ValueType,
        args::{I16Arg, IndexedArg, RegArg, U16Arg},
    },
    simple::{SALState, SimpleAssemblyLanguage, SimpleAssemblyLanguageBase},
};

use crate::{
    NodeVal,
    args::{Immediate, ImmediateI16, ImmediateU16, ShiftConstant},
    indexed::MemoryIndex,
    label::{LabelExpr, RelocPattern},
    opcodes::{self, Opcodes, imm_16},
    reg::Register,
    trans::MipsReloc,
};

use crate::trans::MipsTranslationUnit;

pub struct MipsAssembler<'a> {
    state: SALState<'a, Self>,
}

impl<'a> Default for MipsAssembler<'a> {
    fn default() -> Self {
        Self::new()
    }
}

enum LabelKind {
    IdxSaveMem,
    IdxLoadMem,
    Branch,
    Jump,
    Lui,
    ArithSigned,
    ArithUnsigned,
}

impl<'a> MipsAssembler<'a> {
    pub fn new() -> Self {
        Self {
            state: SALState::new(assembler::simple::Endianess::Big),
        }
    }

    fn instruction(
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
    fn r_type_rd_rs_rt(
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
    fn r_type_rd_rs(
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

    fn r_type_rs_rt(
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

    fn r_type_rd(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        opcode: Opcodes,
    ) {
        let Node(RegArg(rd), node) = ctx.eval(self).coerced(node);

        self.instruction(ctx, node, opcode as u32 + rd.unwrap_or_default().rd());
    }

    fn r_type_rs(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        opcode: Opcodes,
    ) {
        let Node(RegArg(rs), node) = ctx.eval(self).coerced(node);

        self.instruction(ctx, node, opcode as u32 + rs.unwrap_or_default().rs());
    }

    fn r_type_rd_rs_const(
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
            opcode as u32
                + rd.unwrap_or_default().rs()
                + rs.unwrap_or_default().rt()
                + opcodes::rt(shift.0),
        );
    }

    fn i_type_rt_rs_sign(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        opcode: Opcodes,
    ) {
        let Node((RegArg(rt), RegArg(rs), imm), node) = ctx.eval(self).coerced(node);
        let imm = match imm {
            ImmediateI16::Constant(v) => v as u32,
            ImmediateI16::Label(l) => {
                self.immediate(ctx, node, LabelKind::ArithSigned, Immediate::Label(l))
                    .0
            }
        };

        self.instruction(
            ctx,
            node,
            opcode as u32 + rs.unwrap_or_default().rs() + rt.unwrap_or_default().rt() + imm_16(imm),
        );
    }

    fn i_type_rt_rs_unsign(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        opcode: Opcodes,
    ) {
        let Node((RegArg(rt), RegArg(rs), imm), node) = ctx.eval(self).coerced(node);
        let imm = match imm {
            ImmediateU16::Constant(v) => v as u32,
            ImmediateU16::Label(l) => {
                self.immediate(ctx, node, LabelKind::ArithUnsigned, Immediate::Label(l))
                    .0
            }
        };

        self.instruction(
            ctx,
            node,
            opcode as u32 + rs.unwrap_or_default().rs() + rt.unwrap_or_default().rt() + imm_16(imm),
        );
    }

    fn i_type(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        kind: LabelKind,
        opcode: Opcodes,
    ) {
        let Node(immediate, node) = ctx.eval(self).coerced(node);
        let (imm, _) = self.immediate(ctx, node, kind, immediate);
        self.instruction(ctx, node, opcode as u32 + imm_16(imm));
    }

    fn i_type_rt(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        kind: LabelKind,
        opcode: Opcodes,
    ) {
        let Node((RegArg(rt), immediate), node) = ctx.eval(self).coerced(node);
        let (imm, _) = self.immediate(ctx, node, kind, immediate);
        self.instruction(
            ctx,
            node,
            opcode as u32 + rt.unwrap_or_default().rt() + imm_16(imm),
        );
    }

    fn i_type_rt_rs(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        kind: LabelKind,
        opcode: Opcodes,
    ) {
        let Node((RegArg(rt), RegArg(rs), immediate), node) = ctx.eval(self).coerced(node);
        let (imm, _) = self.immediate(ctx, node, kind, immediate);
        self.instruction(
            ctx,
            node,
            opcode as u32 + rs.unwrap_or_default().rs() + rt.unwrap_or_default().rt() + imm_16(imm),
        );
    }

    fn i_type_rs_rt(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        kind: LabelKind,
        opcode: Opcodes,
    ) {
        let Node((RegArg(rs), RegArg(rt), immediate), node) = ctx.eval(self).coerced(node);
        let (imm, _) = self.immediate(ctx, node, kind, immediate);
        self.instruction(
            ctx,
            node,
            opcode as u32 + rs.unwrap_or_default().rs() + rt.unwrap_or_default().rt() + imm_16(imm),
        );
    }

    fn i_type_rt_rs_idx(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        node: NodeRef<'a>,
        kind: LabelKind,
        opcode: Opcodes,
    ) {
        let Node((RegArg(rt), RegArg(rs), IndexedArg(indexed)), node) =
            ctx.eval(self).coerced(node);
        let (imm, _) = self.indexed(ctx, node, kind, indexed.unwrap_or_default());
        self.instruction(
            ctx,
            node,
            opcode as u32 + rs.unwrap_or_default().rs() + rt.unwrap_or_default().rt() + imm_16(imm),
        );
    }

    fn j_type(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        opcode: Opcodes,
    ) {
        let Node(immediate, node) = ctx.eval(self).coerced(node);
        let (imm, _) = self.immediate(ctx, node, LabelKind::Jump, immediate);
        self.instruction(ctx, node, opcode as u32 + opcodes::imm_26(imm));
    }

    fn no_args(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        instruction: u32,
    ) {
        let Node((), node) = ctx.eval(self).coerced(node);
        self.instruction(ctx, node, instruction);
    }

    fn indexed(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        kind: LabelKind,
        indexed: MemoryIndex<'a>,
    ) -> (u32, Register) {
        match indexed {
            MemoryIndex::LabelRegisterOffset(register, label_expr) => todo!(),
            MemoryIndex::RegisterOffset(register, _) => todo!(),
        }
    }

    fn immediate(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        kind: LabelKind,
        immediate: Immediate<'a>,
    ) -> (u32, Register) {
        match kind {
            LabelKind::IdxSaveMem => todo!(),
            LabelKind::IdxLoadMem => todo!(),
            LabelKind::Branch => todo!(),
            LabelKind::Lui => todo!(),
            LabelKind::ArithSigned => todo!(),
            LabelKind::ArithUnsigned => todo!(),
            LabelKind::Jump => match immediate {
                Immediate::Label(l) => {
                    self.current_section_mut(ctx, node)
                        .reloc(MipsReloc {}, Some(ctx.context.node_to_owned(node)));
                    (0, Register::default())
                }
                Immediate::SignedConstant(v) => {
                    if v.is_negative() {
                        ctx.context
                            .report_warning(node, "negative constant used in jump instruction");
                    }
                    (v as u32, Register::default())
                }
                Immediate::UnsignedConstant(v) if v > 0x3FFFFFF => {
                    ctx.context
                        .report_warning(node, "constant cannot fit in 26 bits");
                    (v, Register::default())
                }
                Immediate::UnsignedConstant(v) => (v, Register::default()),
            },
        }
    }
}

impl<'a> SimpleAssemblyLanguage<'a> for MipsAssembler<'a> {
    type Reg = Register;
    type Indexed = MemoryIndex<'a>;
    type CustomValue = EmptyCustomValue<Self>;
    type Label = LabelExpr<'a>;

    type TranslationUnitMachine = MipsTranslationUnit;
    type Usize = u32;
    type Isize = i32;
    type Uptr = u32;
    type Iptr = i32;
    type Ufunc = u32;
    type Ifunc = i32;

    const DEFAULT_INTEGER_POSTFIX: &'a str = "i32";
    const DEFAULT_FLOAT_POSTFIX: &'a str = "f32";

    fn parse_ident(
        &mut self,
        _: &mut ExprCtx<'a, '_, Self>,
        ident: assembler::Node<'a, &'a str>,
        _: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        if let Ok(reg) = Register::from_str(ident.0) {
            Value::Register(reg)
        } else {
            Value::Label(LabelExpr::new(ident.0))
        }
    }

    fn assemble_mnemonic(
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

            "lb" => self.i_type_rt_rs_idx(ctx, node, LabelKind::IdxLoadMem, Opcodes::Lb),
            "lbu" => self.i_type_rt_rs_idx(ctx, node, LabelKind::IdxLoadMem, Opcodes::Lbu),
            "lh" => self.i_type_rt_rs_idx(ctx, node, LabelKind::IdxLoadMem, Opcodes::Lh),
            "lhu" => self.i_type_rt_rs_idx(ctx, node, LabelKind::IdxLoadMem, Opcodes::Lhu),
            "lw" => self.i_type_rt_rs_idx(ctx, node, LabelKind::IdxLoadMem, Opcodes::Lw),
            "lwl" => self.i_type_rt_rs_idx(ctx, node, LabelKind::IdxLoadMem, Opcodes::Lwl),
            "lwr" => self.i_type_rt_rs_idx(ctx, node, LabelKind::IdxLoadMem, Opcodes::Lwr),

            "sb" => self.i_type_rt_rs_idx(ctx, node, LabelKind::IdxSaveMem, Opcodes::Sb),
            "sh" => self.i_type_rt_rs_idx(ctx, node, LabelKind::IdxSaveMem, Opcodes::Sh),
            "sw" => self.i_type_rt_rs_idx(ctx, node, LabelKind::IdxSaveMem, Opcodes::Sw),
            "swl" => self.i_type_rt_rs_idx(ctx, node, LabelKind::IdxSaveMem, Opcodes::Swl),
            "swr" => self.i_type_rt_rs_idx(ctx, node, LabelKind::IdxSaveMem, Opcodes::Swr),

            "j" => self.j_type(ctx, node, Opcodes::J),
            "jal" => self.j_type(ctx, node, Opcodes::Jal),
            "jalr" => self.r_type_rd_rs(ctx, node, Opcodes::Jalr),
            "jr" => self.r_type_rs(ctx, node, Opcodes::Jr),

            "beq" => self.i_type_rt_rs(ctx, node, LabelKind::Branch, Opcodes::Beq),
            "bgez" => self.i_type_rt(ctx, node, LabelKind::Branch, Opcodes::Bgez),
            "bgezal" => self.i_type_rt(ctx, node, LabelKind::Branch, Opcodes::Bgezal),
            "bgtz" => self.i_type_rt(ctx, node, LabelKind::Branch, Opcodes::Bgtz),
            "blez" => self.i_type_rt(ctx, node, LabelKind::Branch, Opcodes::Blez),
            "bltz" => self.i_type_rt(ctx, node, LabelKind::Branch, Opcodes::Bltz),
            "bltzal" => self.i_type_rt(ctx, node, LabelKind::Branch, Opcodes::Bltzal),
            "bne" => self.i_type_rt_rs(ctx, node, LabelKind::Branch, Opcodes::Bne),

            "lui" => self.i_type_rt(ctx, node, LabelKind::Lui, Opcodes::Lui),

            "break" => self.no_args(ctx, node, Opcodes::Break as u32),
            "syscall" => self.no_args(ctx, node, Opcodes::Syscall as u32),

            // pseudo instructions
            "move" => self.r_type_rd_rs(ctx, node, Opcodes::Addu),

            "not" => self.r_type_rd_rs(ctx, node, Opcodes::Nor),

            "ret" => self.no_args(ctx, node, Opcodes::Jr as u32 + Register(31).rs()),

            "b" => self.i_type(ctx, node, LabelKind::Branch, Opcodes::Bgez),
            "bal" => self.i_type(ctx, node, LabelKind::Branch, Opcodes::Bgezal),
            "bnez" => self.i_type_rt(ctx, node, LabelKind::Branch, Opcodes::Bne),

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
                let Node((RegArg(rd), RegArg(rs), IndexedArg(indexed)), node) =
                    ctx.eval(self).coerced(node);
            }
            "usw" => {
                let Node((RegArg(rs), RegArg(rt), IndexedArg(indexed)), node) =
                    ctx.eval(self).coerced(node);
            }

            "la" | "li" => {
                let Node((RegArg(rt), immediate), node): Node<(_, Immediate)> =
                    ctx.eval(self).coerced(node);

                match immediate {
                    Immediate::SignedConstant(signed)
                        if i16::MIN as i32 <= signed && signed <= i16::MAX as i32 =>
                    {
                        self.instruction(
                            ctx,
                            node,
                            Opcodes::Addi as u32
                                + opcodes::imm_16(signed as u32)
                                + rt.unwrap_or_default().rt(),
                        );
                    }
                    Immediate::SignedConstant(signed) => {
                        self.instruction(
                            ctx,
                            node,
                            Opcodes::Lui as u32
                                + opcodes::imm_16(signed as u32 >> 16)
                                + rt.unwrap_or_default().rt(),
                        );
                        if signed as u16 != 0 {
                            self.instruction(
                                ctx,
                                node,
                                Opcodes::Ori as u32
                                    + opcodes::imm_16(signed as u32)
                                    + rt.unwrap_or_default().rt(),
                            );
                        }
                    }
                    Immediate::UnsignedConstant(unsigned) if unsigned <= u16::MAX as u32 => {
                        self.instruction(
                            ctx,
                            node,
                            Opcodes::Ori as u32
                                + opcodes::imm_16(unsigned)
                                + rt.unwrap_or_default().rt(),
                        );
                    }
                    Immediate::UnsignedConstant(unsigned) => {
                        self.instruction(
                            ctx,
                            node,
                            Opcodes::Lui as u32
                                + opcodes::imm_16(unsigned >> 16)
                                + rt.unwrap_or_default().rt(),
                        );
                        if unsigned as u16 != 0 {
                            self.instruction(
                                ctx,
                                node,
                                Opcodes::Ori as u32
                                    + opcodes::imm_16(unsigned)
                                    + rt.unwrap_or_default().rt(),
                            );
                        }
                    }
                    Immediate::Label(label_expr) => {
                        self.instruction(
                            ctx,
                            node,
                            Opcodes::Lui as u32 + rt.unwrap_or_default().rt(),
                        );
                        self.instruction(
                            ctx,
                            node,
                            Opcodes::Ori as u32 + rt.unwrap_or_default().rt(),
                        );
                    }
                }
            }

            _ => ctx.asm(self).unknown_mnemonic(mnemonic, node),
        }
    }

    fn eval_index(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        node: NodeRef<'a>,
        lhs: Option<NodeVal<'a>>,
        opening: NodeRef<'a>,
        rhs: Option<NodeVal<'a>>,
        closing: NodeRef<'a>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        let (Some(lhs), Some(rhs)) = (lhs, rhs) else {
            return ctx
                .eval(self)
                .index_base(node, lhs, opening, rhs, closing, hint);
        };
        match (lhs.0, rhs.0) {
            (Value::Register(r), Value::Constant(c)) | (Value::Constant(c), Value::Register(r)) => {
                Value::Indexed(MemoryIndex::RegisterOffset(
                    r,
                    c.checked_cast_iptr(node, ctx.context).unwrap_or(0),
                ))
            }
            (Value::Indexed(MemoryIndex::RegisterOffset(r, o)), Value::Constant(c))
            | (Value::Constant(c), Value::Indexed(MemoryIndex::RegisterOffset(r, o))) => {
                Value::Indexed(MemoryIndex::RegisterOffset(
                    r,
                    o.wrapping_add(c.checked_cast_iptr(node, ctx.context).unwrap_or(0)),
                ))
            }

            (Value::Label(l), Value::Constant(c)) | (Value::Constant(c), Value::Label(l)) => {
                Value::Label(l.offset(c.checked_cast_iptr(node, ctx.context).unwrap_or(0)))
            }
            (Value::Label(l), Value::Register(r)) | (Value::Register(r), Value::Label(l)) => {
                Value::Indexed(MemoryIndex::LabelRegisterOffset(r, l))
            }

            (Value::Label(l), Value::Indexed(MemoryIndex::RegisterOffset(r, i)))
            | (Value::Indexed(MemoryIndex::RegisterOffset(r, i)), Value::Label(l)) => {
                Value::Indexed(MemoryIndex::LabelRegisterOffset(r, l.offset(i)))
            }
            _ => ctx
                .eval(self)
                .index_base(node, Some(lhs), opening, Some(rhs), closing, hint),
        }
    }

    fn add_value_data(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        value: Value<'a, Self>,
        node: assembler::NodeRef<'a>,
    ) {
        match value {
            Value::Constant(constant) => self.add_constant_data(ctx, constant, node),
            Value::Label(l) => match l.pattern {
                Some(RelocPattern::High | RelocPattern::Low) => {
                    ctx.context.report_error(
                        node,
                        "hi and lo relocation patters cannot be used in static data",
                    );
                    return;
                }
                Some(RelocPattern::U8) => self.add_constant_data(ctx, Constant::U8(0), node),
                Some(RelocPattern::U16) => self.add_constant_data(ctx, Constant::U16(0), node),
                None | Some(RelocPattern::U32) => {
                    self.add_constant_data(ctx, Constant::U32(0), node)
                }
                Some(RelocPattern::U64) => self.add_constant_data(ctx, Constant::U64(0), node),
            },
            _ => ctx
                .context
                .report_error(node, format!("cannot use '{}' as data", value.get_type())),
        }
    }

    fn state_mut(&mut self) -> &mut SALState<'a, Self> {
        &mut self.state
    }

    fn state(&self) -> &SALState<'a, Self> {
        &self.state
    }
}
