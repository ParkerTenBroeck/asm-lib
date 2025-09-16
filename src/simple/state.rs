use std::collections::HashMap;

use crate::Number;
use crate::context::Context;
use crate::simple::SimpleAssemblyLanguage;
use crate::simple::trans::TranslationUnit;
use crate::{context::NodeRef, logs::LogEntry};

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum Endianess {
    Little,
    Big,
}

#[derive(Clone, Copy, Debug)]
pub struct NumericLabel<'a> {
    pub count: u32,
    pub back: Option<&'a str>,
    pub front: &'a str,
}

impl<'a> NumericLabel<'a> {
    pub fn new(ctx: &mut Context<'a>, num: u32) -> Self {
        Self {
            count: 0,
            back: None,
            front: Self::str(ctx, 0, num),
        }
    }

    fn str(ctx: &mut Context<'a>, count: u32, num: u32) -> &'a str {
        ctx.alloc_str(format!(".L{num}^B{count}"))
    }

    pub fn bind<L: SimpleAssemblyLanguage<'a>>(
        &mut self,
        ctx: &mut Context<'a>,
        num: u32,
        state: &mut SALState<'a, L>,
        node: NodeRef<'a>,
    ) {
        self.back = Some(self.front);
        let section = state.expect_section(ctx, node);
        let node_owned = ctx.node_to_owned(node);
        let res = state
            .trans
            .resolve_mut(section)
            .bind_symbol_resolve(self.front, Some(node_owned.clone()));
        if let Err(err) = res {
            ctx.report_owned(err.to_log_entry(self.front, node_owned.clone()));
        }
        let res = state.trans.set_symbol_visibility(
            self.front,
            super::trans::sym::SymbolVisibility::Local,
            Some(node_owned.clone()),
        );
        if let Err(err) = res {
            ctx.report_owned(err.to_log_entry(self.front, node_owned));
        }
        self.count += 1;
        self.front = Self::str(ctx, self.count, num)
    }
}

#[derive(Clone, Debug)]
pub struct SALState<'a, L: SimpleAssemblyLanguage<'a>> {
    pub current_section: Option<&'a str>,
    pub last_non_local_label: Option<&'a str>,
    pub endianess: Endianess,
    pub trans: TranslationUnit<L::TranslationUnitMachine>,
    pub local_numeric_labels: HashMap<u32, NumericLabel<'a>>,
}

impl<'a, L: SimpleAssemblyLanguage<'a>> SALState<'a, L> {
    pub fn new(endianess: Endianess) -> Self {
        Self {
            current_section: None,
            last_non_local_label: None,
            endianess,
            trans: TranslationUnit::new(),
            local_numeric_labels: HashMap::new(),
        }
    }
    pub fn expect_section(&mut self, context: &mut Context<'a>, node: NodeRef<'a>) -> &'a str {
        if self.current_section.is_none() {
            context.report(LogEntry::new()
                .error(node, "section not specified, defaulting to .text")
                .hint_locless("specify section with .text .data .rodata .bss or .section \"<name>\" directives")
            );
            self.current_section = Some(".text");
        }
        self.current_section.unwrap()
    }

    pub fn expect_last_label(
        &mut self,
        context: &mut Context<'a>,
        node: NodeRef<'a>,
    ) -> Option<&'a str> {
        if self.last_non_local_label.is_none() {
            context.report(LogEntry::new()
                    .error(node, "encountered local label before non local label")
                    .hint_locless("local labels start with '.' consider adding a non local label before the definition of this one")
            );
        }

        self.last_non_local_label
    }

    pub fn expect_back_numeric_label(
        &mut self,
        context: &mut Context<'a>,
        node: NodeRef<'a>,
        num: &'a str,
    ) -> Option<&'a str> {
        todo!()
    }

    pub fn expect_front_numeric_label(
        &mut self,
        context: &mut Context<'a>,
        node: NodeRef<'a>,
        num: &'a str,
    ) -> &'a str {
        todo!()
    }

    pub fn bind_local_numeric_label(
        &mut self,
        context: &mut Context<'a>,
        node: NodeRef<'a>,
        num: &'a str,
    ) -> &'a str{
        todo!()
    }
}
