use crate::context::Context;
use crate::simple::SimpleAssemblyLanguage;
use crate::simple::trans::TranslationUnit;
use crate::{context::NodeRef, logs::LogEntry};

#[derive(Clone, Copy, Eq, PartialEq, Debug, Default)]
pub enum Endianess {
    #[default]
    Little,
    Big,
}

#[derive(Clone, Debug, Default)]
pub struct SALState<'a, L: SimpleAssemblyLanguage<'a>> {
    pub current_section: Option<&'a str>,
    pub last_non_local_label: Option<&'a str>,
    pub endianess: Endianess,
    pub trans: TranslationUnit<L::TranslationUnitMachine>,
}

impl<'a, L: SimpleAssemblyLanguage<'a>> SALState<'a, L> {
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
}
