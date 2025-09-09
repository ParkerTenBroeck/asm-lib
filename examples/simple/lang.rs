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

pub enum LabelKind {
    IdxSaveMem,
    IdxLoadMem,
    Branch,
    Jump,
    Lui,
    ArithSigned,
    ArithUnsigned,
    La,
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
            Value::Label(l) => {
                let Some(calculation) = l.ty.reloc_type(ctx.asm(self), false) else {
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

    fn state_mut(&mut self) -> &mut SALState<'a, Self> {
        &mut self.state
    }

    fn state(&self) -> &SALState<'a, Self> {
        &self.state
    }
}
