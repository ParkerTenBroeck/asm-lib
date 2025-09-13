use std::str::FromStr;

use assembler::{
    NodeRef,
    expression::{Constant, EmptyCustomValue, ExprCtx, Value, ValueType},
    simple::{SALState, SimpleAssemblyLanguage, SimpleAssemblyLanguageBase},
};

use crate::{
    NodeVal,
    indexed::MemoryIndex,
    label::{LabelExpr, RelocPattern},
    reg::Register,
    trans::{MipsReloc, MipsRelocPattern},
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
        self.assemble_mnemonic_impl(ctx, mnemonic, node);
    }

    fn eval_binop(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        node: NodeRef<'a>,
        lhs: assembler::expression::NodeVal<'a, Self>,
        op: assembler::Node<'a, assembler::expression::binop::BinOp>,
        rhs: assembler::expression::NodeVal<'a, Self>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        self.eval_binop_impl(ctx, node, lhs, op, rhs, hint)
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
        self.eval_index_impl(ctx, node, lhs, opening, rhs, closing, hint)
    }

    fn add_value_data(
        &mut self,
        ctx: &mut assembler::LangCtx<'a, '_, Self>,
        value: Value<'a, Self>,
        node: assembler::NodeRef<'a>,
    ) {
        match value {
            Value::Constant(constant) => self.add_constant_data(ctx, constant, node),
            Value::Label(l) => {
                let Some(calculation) = l.ty.reloc_type(&mut ctx.asm(self), false) else {
                    return;
                };
                let (pattern, constant) = match l.pattern {
                    Some(RelocPattern::High | RelocPattern::Low) => {
                        ctx.context.report_error(
                            node,
                            "hi and lo relocation patters cannot be used in static data",
                        );
                        return;
                    }
                    Some(RelocPattern::U8) => (MipsRelocPattern::U8, Constant::U8(0)),
                    Some(RelocPattern::U16) => (MipsRelocPattern::U16, Constant::U16(0)),
                    None | Some(RelocPattern::U32) => (MipsRelocPattern::U32, Constant::U32(0)),
                };
                self.current_section_mut(ctx, node).reloc(
                    MipsReloc {
                        pattern,
                        calculation,
                        offset: l.offset,
                        overflow: false,
                    },
                    Some(ctx.context.node_to_owned(node)),
                );
                self.add_constant_data(ctx, constant, node)
            }
            _ => ctx
                .context
                .report_error(node, format!("cannot use '{}' as data", value.get_type())),
        }
    }

    fn eval_func(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        func: assembler::expression::FuncParamParser<'a, '_>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        self.eval_func_impl(ctx, func, hint)
    }

    fn state_mut(&mut self) -> &mut SALState<'a, Self> {
        &mut self.state
    }

    fn state(&self) -> &SALState<'a, Self> {
        &self.state
    }
}
