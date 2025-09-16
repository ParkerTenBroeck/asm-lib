use std::collections::HashMap;

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

    pub fn next(&mut self, ctx: &mut Context<'a>, num: u32) -> &'a str {
        self.back = Some(self.front);
        let label = self.front;
        self.count += 1;
        self.front = Self::str(ctx, self.count, num);
        label
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
        let num = Self::parse_label_number(context, node, num)?;
        let entry = self
            .local_numeric_labels
            .entry(num)
            .or_insert_with(|| NumericLabel::new(context, num));
        if entry.back.is_none() {
            context.report_error(node, "label is not defined");
        }
        entry.back
    }

    pub fn expect_front_numeric_label(
        &mut self,
        context: &mut Context<'a>,
        node: NodeRef<'a>,
        num: &'a str,
    ) -> Option<&'a str> {
        let num = Self::parse_label_number(context, node, num)?;
        let entry = self
            .local_numeric_labels
            .entry(num)
            .or_insert_with(|| NumericLabel::new(context, num));
        Some(entry.front)
    }

    pub fn expect_next_local_numeric_label(
        &mut self,
        context: &mut Context<'a>,
        node: NodeRef<'a>,
        num: &'a str,
    ) -> Option<&'a str> {
        let num = Self::parse_label_number(context, node, num)?;
        let entry = self
            .local_numeric_labels
            .entry(num)
            .or_insert_with(|| NumericLabel::new(context, num));
        Some(entry.next(context, num))
    }

    fn parse_label_number(
        context: &mut Context<'a>,
        node: NodeRef<'a>,
        num: &'a str,
    ) -> Option<u32> {
        match num.parse() {
            Ok(num) => Some(num),
            Err(err) => {
                match err.kind() {
                    std::num::IntErrorKind::Empty => {
                        context.report_error(node, "numeric label is empty")
                    }
                    std::num::IntErrorKind::InvalidDigit => {
                        context.report_error(node, "invalid digit in numeric label")
                    }
                    std::num::IntErrorKind::PosOverflow => {
                        context.report_error(node, "numeric label number too large")
                    }
                    std::num::IntErrorKind::NegOverflow => {
                        context.report_error(node, "numeric label number too small")
                    }
                    std::num::IntErrorKind::Zero => {
                        context.report_error(node, "numeric label number cannot be zero")
                    }
                    _ => context.report_error(node, format!("error: {err}")),
                }
                None
            }
        }
    }
}
