use crate::{
    ansi::*,
    simple::trans::reloc::{Reloc, Relocations},
};
use num_traits::PrimInt;
use std::fmt::LowerHex;

use crate::simple::trans::{
    TranslationUnit, TranslationUnitMachine,
    section::Section,
    sym::{Symbol, SymbolIdx, Symbols},
};

impl<T: TranslationUnitMachine<PtrSizeType: LowerHex>> std::fmt::Display for TranslationUnit<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.symbols.fmt(f, self)?;

        for section in &self.sections {
            section.fmt(f, self)?
        }
        Ok(())
    }
}

impl<T: TranslationUnitMachine> Section<T> {
    pub fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        trans: &super::TranslationUnit<T>,
    ) -> std::fmt::Result {
        self.relocations.fmt(f, trans);
        Ok(())
    }
}

impl<T: Reloc> Relocations<T> {
    pub fn fmt<M: super::TranslationUnitMachine<Reloc = T>>(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        trans: &super::TranslationUnit<M>,
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
            let list_size = trans.symbols.len().ilog10() as usize + 1;
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
        let list_size = trans.symbols.len().ilog10() as usize + 1;
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
