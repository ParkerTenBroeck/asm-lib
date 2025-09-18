use crate::{
    LogEntry,
    assembler::LangCtx,
    context::NodeRef,
    expression::Constant,
    simple::trans::{SectionMut, data::PushDataResult},
};

use super::*;

pub trait SimpleAssemblyLanguageBase<'a>: SimpleAssemblyLanguage<'a> {
    fn add_constant_data(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        constant: Constant<'a, Self>,
        node: NodeRef<'a>,
    ) {
        let align = constant.get_align().unwrap_or_default();
        macro_rules! dat {
            ($expr:expr) => {
                self.add_data(ctx, $expr, align, node)
            };
        }
        use num_traits::ToBytes;
        match self.state_mut().endianess {
            Endianess::Little => match constant {
                Constant::I8(v) => dat!(&v.to_le_bytes()),
                Constant::I16(v) => dat!(&v.to_le_bytes()),
                Constant::I32(v) => dat!(&v.to_le_bytes()),
                Constant::I64(v) => dat!(&v.to_le_bytes()),
                Constant::I128(v) => dat!(&v.to_le_bytes()),
                Constant::Isize(v) => dat!(v.to_le_bytes().as_ref()),
                Constant::Iptr(v) => dat!(v.to_le_bytes().as_ref()),
                Constant::Ifunc(v) => dat!(v.to_le_bytes().as_ref()),
                Constant::U8(v) => dat!(&v.to_le_bytes()),
                Constant::U16(v) => dat!(&v.to_le_bytes()),
                Constant::U32(v) => dat!(&v.to_le_bytes()),
                Constant::U64(v) => dat!(&v.to_le_bytes()),
                Constant::U128(v) => dat!(&v.to_le_bytes()),
                Constant::Usize(v) => dat!(v.to_le_bytes().as_ref()),
                Constant::Uptr(v) => dat!(v.to_le_bytes().as_ref()),
                Constant::Ufunc(v) => dat!(v.to_le_bytes().as_ref()),
                Constant::F32(v) => dat!(&v.to_le_bytes()),
                Constant::F64(v) => dat!(&v.to_le_bytes()),
                Constant::Str(v) => match v {
                    crate::expression::AsmStr::Str(str) => dat!(str.as_bytes()),
                    crate::expression::AsmStr::ByteStr(str) => dat!(str),
                    crate::expression::AsmStr::CStr(str) => {
                        self.add_data(ctx, str, align, node);
                        self.add_data(ctx, &[0], num_traits::one(), node)
                    }
                },
                Constant::Char(v) => dat!(&(v as u32).to_le_bytes()),
                Constant::Bool(v) => dat!(&(v as u8).to_le_bytes()),
            },
            Endianess::Big => match constant {
                Constant::I8(v) => dat!(&v.to_be_bytes()),
                Constant::I16(v) => dat!(&v.to_be_bytes()),
                Constant::I32(v) => dat!(&v.to_be_bytes()),
                Constant::I64(v) => dat!(&v.to_be_bytes()),
                Constant::I128(v) => dat!(&v.to_be_bytes()),
                Constant::Isize(v) => dat!(v.to_be_bytes().as_ref()),
                Constant::Iptr(v) => dat!(v.to_be_bytes().as_ref()),
                Constant::Ifunc(v) => dat!(v.to_be_bytes().as_ref()),
                Constant::U8(v) => dat!(&v.to_be_bytes()),
                Constant::U16(v) => dat!(&v.to_be_bytes()),
                Constant::U32(v) => dat!(&v.to_be_bytes()),
                Constant::U64(v) => dat!(&v.to_be_bytes()),
                Constant::U128(v) => dat!(&v.to_be_bytes()),
                Constant::Usize(v) => dat!(v.to_be_bytes().as_ref()),
                Constant::Uptr(v) => dat!(v.to_be_bytes().as_ref()),
                Constant::Ufunc(v) => dat!(v.to_be_bytes().as_ref()),
                Constant::F32(v) => dat!(&v.to_be_bytes()),
                Constant::F64(v) => dat!(&v.to_be_bytes()),
                Constant::Str(v) => match v {
                    crate::expression::AsmStr::Str(str) => dat!(str.as_bytes()),
                    crate::expression::AsmStr::ByteStr(str) => dat!(str),
                    crate::expression::AsmStr::CStr(str) => {
                        self.add_data(ctx, str, align, node);
                        self.add_data(ctx, &[0], num_traits::one(), node)
                    }
                },
                Constant::Char(v) => dat!(&(v as u32).to_be_bytes()),
                Constant::Bool(v) => dat!(&(v as u8).to_be_bytes()),
            },
        }
    }

    fn add_data(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        data: &[u8],
        align: Self::Uptr,
        node: NodeRef<'a>,
    ) {
        let result = self.current_section_mut(ctx, node).data(
            data,
            align,
            Some(ctx.context.node_to_owned(node)),
        );
        self.report_data_result(ctx, result, align, node);
    }

    fn add_space_data(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        space: Self::Uptr,
        align: Self::Uptr,
        node: NodeRef<'a>,
    ) {
        let result = self.current_section_mut(ctx, node).space(
            space,
            align,
            Some(ctx.context.node_to_owned(node)),
        );
        self.report_data_result(ctx, result, align, node);
    }

    fn add_align(&mut self, ctx: &mut LangCtx<'a, '_, Self>, align: Self::Uptr, node: NodeRef<'a>) {
        let result = self
            .current_section_mut(ctx, node)
            .align(align, Some(ctx.context.node_to_owned(node)))
            .ignore_warning();
        self.report_data_result(ctx, result, align, node);
    }

    fn set_section(&mut self, _: &mut LangCtx<'a, '_, Self>, section: &'a str, _: NodeRef<'a>) {
        self.state_mut().current_section = Some(section);
    }

    fn current_section_mut<'b>(
        &'b mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        node: NodeRef<'a>,
    ) -> SectionMut<'b, Self::TranslationUnitMachine>
    where
        'a: 'b,
    {
        let section = self.state_mut().expect_section(ctx.context, node);
        self.state_mut().trans.resolve_mut(section)
    }
}

trait PrivateSimpleAssemblyLanguageBase<'a>: SimpleAssemblyLanguage<'a> {
    fn report_data_result<T>(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        result: PushDataResult<T>,
        align: Self::Uptr,
        node: NodeRef<'a>,
    ) {
        match result {
            PushDataResult::Ok(_) => {}
            PushDataResult::WarningAlign(_) => {
                ctx.context.report(
                    LogEntry::new()
                        .warning(node, "data is implicitly aligned")
                        .hint_locless(format!(
                            "consider adding '.align {align}' before defining the value"
                        )),
                );
            }
            PushDataResult::NotEnoughSpace => {
                ctx.context.report_error(
                    node,
                    format!(
                        "section '{}' exceeded maximum size",
                        self.state()
                            .current_section
                            .unwrap_or_default()
                            .escape_debug()
                    ),
                );
            }
        }
    }
}

impl<'a, T: SimpleAssemblyLanguage<'a>> SimpleAssemblyLanguageBase<'a> for T {}
impl<'a, T: SimpleAssemblyLanguage<'a>> PrivateSimpleAssemblyLanguageBase<'a> for T {}
