use std::str::FromStr;

use assembler::{
    Node, NodeRef,
    expression::{Constant, EmptyCustomValue, ExprCtx, Value, ValueType, args::RegArg},
    simple::{SALState, SimpleAssemblyLanguage, SimpleAssemblyLanguageBase},
};

use crate::{
    NodeVal,
    args::{Immediate, ShiftConstant},
    indexed::MemoryIndex,
    label::{LabelExpr, RelocPattern},
    opcodes::{self, Opcodes},
    reg::Register,
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
        let section = self.state.expect_section(ctx.context, node);
        let section = self.state.trans.resolve_or_make(section);
        let node = Some(ctx.context.node_to_owned(node));

        self.state
            .trans
            .get_mut(section)
            .data(&ins.to_be_bytes(), 4, node);
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

    fn i_type_rd_rs(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        opcode: Opcodes,
    ) {
        let Node((RegArg(rd), RegArg(rs), immediate), node) = ctx.eval(self).coerced(node);

        match immediate {
            Immediate::Label(l) => {}
            Immediate::SignedConstant(v) => {}
            Immediate::UnsignedConstant(v) => {}
        }

        self.instruction(
            ctx,
            node,
            opcode as u32 + rd.unwrap_or_default().rd() + rs.unwrap_or_default().rs(),
        );
    }

    fn i_type_rs_rt(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        opcode: Opcodes,
    ) {
        let Node((RegArg(rs), RegArg(rt), immediate), node) = ctx.eval(self).coerced(node);

        match immediate {
            Immediate::Label(l) => {}
            Immediate::SignedConstant(v) => {}
            Immediate::UnsignedConstant(v) => {}
        }

        self.instruction(
            ctx,
            node,
            opcode as u32 + rs.unwrap_or_default().rs() + rt.unwrap_or_default().rt(),
        );
    }

    fn i_type_rd(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        node: assembler::NodeRef<'a>,
        opcode: Opcodes,
    ) {
        let Node((RegArg(rd), immediate), node) = ctx.eval(self).coerced(node);

        match immediate {
            Immediate::Label(l) => {}
            Immediate::SignedConstant(v) => {}
            Immediate::UnsignedConstant(v) => {}
        }

        self.instruction(ctx, node, opcode as u32 + rd.unwrap_or_default().rd());
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

            "addi" => self.i_type_rd_rs(ctx, node, Opcodes::Addi),
            "addiu" => self.i_type_rd_rs(ctx, node, Opcodes::Addiu),
            "ori" => self.i_type_rd_rs(ctx, node, Opcodes::Ori),
            "xori" => self.i_type_rd_rs(ctx, node, Opcodes::Xori),
            "andi" => self.i_type_rd_rs(ctx, node, Opcodes::Andi),

            "lb" => self.i_type_rd_rs(ctx, node, Opcodes::Lb),
            "lbu" => self.i_type_rd_rs(ctx, node, Opcodes::Lbu),
            "lh" => self.i_type_rd_rs(ctx, node, Opcodes::Lh),
            "lhu" => self.i_type_rd_rs(ctx, node, Opcodes::Lhu),
            "lw" => self.i_type_rd_rs(ctx, node, Opcodes::Lw),
            "lwl" => self.i_type_rd_rs(ctx, node, Opcodes::Lwl),
            "lwr" => self.i_type_rd_rs(ctx, node, Opcodes::Lwr),

            "slit" => self.i_type_rd_rs(ctx, node, Opcodes::Slti),
            "sltiu" => self.i_type_rd_rs(ctx, node, Opcodes::Sltiu),

            "sb" => self.i_type_rs_rt(ctx, node, Opcodes::Sb),
            "sh" => self.i_type_rs_rt(ctx, node, Opcodes::Sh),
            "sw" => self.i_type_rs_rt(ctx, node, Opcodes::Sw),
            "swl" => self.i_type_rs_rt(ctx, node, Opcodes::Swl),
            "swr" => self.i_type_rs_rt(ctx, node, Opcodes::Swr),

            // "j" => self.j_type(ctx, node, Opcodes::J),
            // "jal" => self.j_type(ctx, node, Opcodes::Jal),
            // "jalr" => self.i_type_rs_rt(ctx, node, Opcodes::Jalr),
            // "jr" => self.i_type_rs_rt(ctx, node, Opcodes::Jr),

            // "beq" => self.i_type_rs_rt(ctx, node, Opcodes::Beq),
            // "bgez" => self.i_type_rs_rt(ctx, node, Opcodes::Bgez),
            // "bgezal" => self.i_type_rs_rt(ctx, node, Opcodes::Bgezal),
            // "bgtz" => self.i_type_rs_rt(ctx, node, Opcodes::Bgtz),
            // "blez" => self.i_type_rs_rt(ctx, node, Opcodes::Blez),
            // "bltz" => self.i_type_rs_rt(ctx, node, Opcodes::Bltz),
            // "bltzal" => self.i_type_rs_rt(ctx, node, Opcodes::Bltzal),
            // "bne" => self.i_type_rs_rt(ctx, node, Opcodes::Bne),
            "lui" => self.i_type_rd(ctx, node, Opcodes::Lui),

            "break" => self.instruction(ctx, node, Opcodes::Break as u32),
            "syscall" => self.instruction(ctx, node, Opcodes::Syscall as u32),

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
