use crate::{
    ansi::{self, *},
    simple::trans::reloc::{Reloc, Relocations},
};
use num_traits::PrimInt;
use std::fmt::{Formatter, LowerHex, Write};

use crate::simple::trans::{
    TranslationUnit, TranslationUnitMachine,
    section::Section,
    sym::{Symbol, SymbolIdx, Symbols},
};

impl<T: TranslationUnitMachine<PtrSizeType: LowerHex>> std::fmt::Display for TranslationUnit<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.symbols.fmt(f, self)?;
        writeln!(f)?;
        for section in &self.sections {
            section.fmt(f, self)?
        }
        Ok(())
    }
}

impl<T: TranslationUnitMachine<PtrSizeType: LowerHex>> Section<T> {
    pub fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        trans: &super::TranslationUnit<T>,
    ) -> std::fmt::Result {
        let name = trans
            .str_table
            .get(self.name())
            .unwrap_or_default()
            .escape_debug();
        let size = self.data.current_offset();
        let align = self.data.align();

        let ptr_size = std::mem::size_of::<T::PtrSizeType>() * 2;

        let r = if self.readable { "r" } else { "_" };
        let w = if self.writeable { "w" } else { "_" };
        let x = if self.executable { "x" } else { "_" };
        writeln!(
            f,
            "section size:{size:0>ptr_size$x}, align:{align:0>ptr_size$x}, rwx:{r}{w}{x} '{name}'"
        )?;
        writeln!(f)?;

        if self.executable {
            T::fmt_section_disassembly(self, trans, f)
        } else {
            fmt_section_hex(self, trans, f)
        }
    }
}

pub fn fmt_section_hex<T: TranslationUnitMachine<PtrSizeType: LowerHex> + ?Sized>(
    section: &Section<T>,
    trans: &TranslationUnit<T>,
    f: &mut Formatter<'_>,
) -> std::fmt::Result {
    let mut offset = 0;
    let chunk_size = 16;
    let ptr_size = std::mem::size_of::<T::PtrSizeType>() * 2;

    for chunk in section.data.slice().chunks(chunk_size) {
        write!(f, "{offset:0>ptr_size$x}: ")?;

        for byte in chunk {
            let color = match byte {
                0x00 => ansi::RESET,
                0x20..=0x7e => ansi::GREEN,
                0x09 | 0x0a | 0x0d => ansi::YELLOW,
                0xff => ansi::BLUE,
                _ => ansi::RED,
            };
            use ansi::RESET;
            write!(f, "{color}{byte:0>2x}{RESET} ")?;
        }
        for _ in chunk.len()..chunk_size {
            write!(f, "   ")?;
        }

        for byte in chunk {
            let (color, char) = match byte {
                0x00 => (ansi::RESET, '.'),
                0x20..=0x7e => (ansi::GREEN, *byte as char),
                0x09 | 0x0a | 0x0d => (ansi::YELLOW, '.'),
                0xff => (ansi::BLUE, '.'),
                _ => (ansi::RED, '.'),
            };
            use ansi::RESET;
            write!(f, "{color}{char}{RESET}")?;
        }
        for _ in chunk.len()..chunk_size {
            write!(f, " ")?;
        }

        writeln!(f)?;
        offset += chunk.len();
    }

    writeln!(f)?;
    section.relocations.fmt(f, trans)?;
    Ok(())
}

impl<T: TranslationUnitMachine + ?Sized> Relocations<T> {
    pub fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        trans: &super::TranslationUnit<T>,
    ) -> std::fmt::Result {
        Ok(())
    }
}

impl<T: PrimInt + LowerHex> Symbol<T> {
    fn fmt<M: TranslationUnitMachine<PtrSizeType = T>>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        idx: SymbolIdx,
        trans: &TranslationUnit<M>,
    ) -> std::fmt::Result {
        let Symbol {
            section,
            ty: kind,
            visibility,
            size,
            offset,
            ..
        } = self;
        let name = trans.str_table.get(self.name()).unwrap_or_default();
        let name = name.escape_debug();
        let int_size = std::mem::size_of::<T>() * 2;
        if let Some(section_idx) = section {
            let section = trans.get(*section_idx);
            let section_name = section.name();
            let section_name = trans.str_table.get(section_name).unwrap_or_default();
            let section_name = "\"".to_owned() + section_name + "\"";
            write!(
                f,
                "{offset:0>int_size$x} {size: >int_size$x} {kind:10} {visibility:6} {section_name: <10} \"{name}\"",
            )?;
        } else {
            write!(
                f,
                "{offset:0>int_size$x} {size: >int_size$x} {kind:10} {visibility:6} None       \"{name}\""
            )?;
        }
        if let Some(dbg) = trans.symbols.get_symbol_dbg(idx) {
            let list_size = trans
                .symbols
                .len()
                .checked_ilog10()
                .map(|v| v as usize + 1)
                .unwrap_or(0);
            if let Some(def) = &dbg.definition {
                write!(
                    f,
                    "\n{FAINT}{:list_size$}   -definition: {def} -> {:?}{RESET}",
                    "",
                    def.src_slice()
                )?;
            }
            if let Some(def) = &dbg.size {
                write!(
                    f,
                    "\n{FAINT}{:list_size$}   -size: {def} -> {:?}{RESET}",
                    "",
                    def.src_slice()
                )?;
            }
            if let Some(def) = &dbg.ty {
                write!(
                    f,
                    "\n{FAINT}{:list_size$}   -type: {def} -> {:?}{RESET}",
                    "",
                    def.src_slice()
                )?;
            }
            if let Some(def) = &dbg.visibility {
                write!(
                    f,
                    "\n{FAINT}{:list_size$}   -visability: {def} -> {:?}{RESET}",
                    "",
                    def.src_slice()
                )?;
            }
        }
        Ok(())
    }
}

impl<T: PrimInt + LowerHex> Symbols<T> {
    pub(crate) fn fmt<M: TranslationUnitMachine<PtrSizeType = T>>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        trans: &TranslationUnit<M>,
    ) -> std::fmt::Result {
        writeln!(f, "symbol table")?;
        let int_size = std::mem::size_of::<T>() * 2;
        let offset = "Offset";
        let size = "Size";
        let kind = "Type";
        let visibility = "Vis";
        let section = "Section";
        let name = "Name";
        let list_size = trans
            .symbols
            .len()
            .checked_ilog10()
            .map(|v| v as usize + 1)
            .unwrap_or(0);
        writeln!(
            f,
            "{:list_size$}  {offset: >int_size$} {size: >int_size$} {kind:10} {visibility:6} {section:10} {name}",
            ""
        )?;
        for (idx, symbol) in self.symbols() {
            write!(f, "{: >list_size$}: ", idx)?;
            symbol.fmt(f, idx, trans)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

pub struct Indent<O: Write> {
    out: O,
    indent: usize,
    nl: bool,
}

impl<O: Write> Indent<O> {
    pub fn new_middle(out: O, indent: usize) -> Self {
        Self {
            out,
            indent,
            nl: false,
        }
    }

    pub fn new_start(out: O, indent: usize) -> Self {
        Self {
            out,
            indent,
            nl: true,
        }
    }
}

impl<O: Write> std::fmt::Write for Indent<O> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        for nl in s.split_inclusive("\n") {
            if self.nl {
                let indent = self.indent;
                write!(self.out, "{: <indent$}", "")?;
            }
            self.out.write_str(nl)?;
            self.nl = nl.ends_with('\n');
        }
        Ok(())
    }
}
