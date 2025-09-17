use crate::{
    ansi::{self, *},
    expression::conversion::FromAsPrimitive,
    simple::trans::{SectionIdx, reloc::Reloc, sym::SymbolVisibility},
};
use num_traits::{PrimInt, WrappingAdd};
use std::fmt::{LowerHex, Write};

use crate::simple::trans::{
    TranslationUnit, TranslationUnitMachine,
    section::Section,
    sym::{Symbol, SymbolIdx, Symbols},
};

impl<T: TranslationUnitMachine<PtrSizeType: LowerHex>> std::fmt::Display for TranslationUnit<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.symbols.display(f, self)?;
        writeln!(f)?;
        for (idx, section) in self.sections() {
            section.display(idx, &mut *f, self)?
        }
        Ok(())
    }
}

impl<T: TranslationUnitMachine<PtrSizeType: LowerHex>> Section<T> {
    pub fn display(
        &self,
        idx: SectionIdx,
        f: &mut impl std::fmt::Write,
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
            "section size:{size:0>ptr_size$x}, align:{align:0>ptr_size$x}, rwx:{r}{w}{x} '{name}'\n"
        )?;

        let mut f = Indent::new_start(f, 2);
        let f = &mut f;

        {
            let mut f = Indent::new_start(&mut *f, 2);
            let f = &mut f;
            if self.executable {
                T::fmt_section_disassembly(idx, self, trans, f)?;
            } else {
                fmt_section_hex(self, trans, f)?;
            }
        }
        {
            writeln!(f, "relocations\n")?;

            let mut f = Indent::new_start(&mut *f, 2);
            let f = &mut f;
            indexed_list_display(
                self.relocations.relocs(),
                f,
                format_args!("{: >ptr_size$} kind", "offset"),
                |(idx, offset, reloc), f| {
                    write!(f, "{offset:0>ptr_size$x} ")?;
                    reloc.display(f, trans)?;
                    if let Some(dbg) = self.debug_info.resolve_reloc_dbg(idx) {
                        let src = dbg.src_slice();
                        write!(f, "\n{FAINT}   ^: def:  {dbg} -> {src:?}{RESET}",)?;
                    }
                    Ok(())
                },
            )?;
        }

        writeln!(f)?;
        Ok(())
    }
}

pub fn fmt_section_disassembly<
    F: std::fmt::Write,
    T: TranslationUnitMachine<PtrSizeType: LowerHex + FromAsPrimitive<usize> + WrappingAdd> + ?Sized,
>(
    section_idx: SectionIdx,
    section: &Section<T>,
    trans: &TranslationUnit<T>,
    f: &mut F,

    max_ins_size: usize,
    mut size: impl FnMut(&Section<T>, &TranslationUnit<T>, std::slice::Iter<'_, u8>) -> usize,
    mut disassemble: impl FnMut(&Section<T>, &TranslationUnit<T>, &mut F, &[u8]) -> std::fmt::Result,
) -> std::fmt::Result {
    let mut iter = section.data.slice().iter();
    let ptr_size = std::mem::size_of::<T::PtrSizeType>() * 2;

    let mut first = true;

    let mut offset = 0;
    let mut data_offset: T::PtrSizeType = num_traits::zero();
    while iter.len() > 0 {
        let size = size(section, trans, iter.clone());

        let start = offset;
        let data_start = data_offset;
        let data_end = data_offset.wrapping_add(&FromAsPrimitive::from_as(size));

        for (_, symbol) in trans.get_symbols(section_idx, data_start..data_end) {
            if !matches!(symbol.visibility, SymbolVisibility::Local) && !first {
                writeln!(f)?;
                break;
            }
        }
        first = false;
        for (_, symbol) in trans.get_symbols(section_idx, data_start..data_end) {
            write!(f, "{:0>ptr_size$x}: ", symbol.offset)?;
            let name = trans.str_table.get(symbol.name()).unwrap_or_default();
            writeln!(f, "<{}>", name.escape_debug())?;
        }

        for (_, comment) in section.debug_info().resolve_comments(data_start..data_end) {
            writeln!(f, "   {FAINT}v: comt: {comment:?}{RESET}")?;
        }

        write!(f, "{: >ptr_size$x}: ", offset)?;

        for _ in 0..size {
            if let Some(byte) = iter.next() {
                offset += 1;
                data_offset = data_offset.wrapping_add(&num_traits::one());
                write!(f, "{byte:0>2x} ")?;
            } else {
                write!(f, "   ")?;
            }
        }
        for _ in size..max_ins_size {
            write!(f, "   ")?;
        }

        disassemble(section, trans, f, &section.data.slice()[start..offset])?;

        for (_, _, reloc) in section.relocations().find_range(data_start..data_offset) {
            write!(f, "    ")?;
            reloc.display(f, trans)?;
        }

        let data_locs = section.debug_info.resolve_data_dbg(data_start..data_offset);
        for (i, dbg) in data_locs.iter().enumerate() {
            if dbg.range.contains(&data_offset) && dbg.range.end != data_offset {
                continue;
            }
            let src = dbg.node.src_slice();
            writeln!(f)?;
            write!(f, "   {FAINT}")?;
            if i + 1 == data_locs.len() {
                write!(f, "{UNDERLINE}")?;
            }
            write!(f, "^: def:  {} -> {src:?}{RESET}", dbg.node)?;
        }

        writeln!(f)?;
    }
    writeln!(f)?;

    Ok(())
}

pub fn fmt_section_hex<T: TranslationUnitMachine<PtrSizeType: LowerHex> + ?Sized>(
    section: &Section<T>,
    _: &TranslationUnit<T>,
    f: &mut impl std::fmt::Write,
) -> std::fmt::Result {
    let mut offset = 0;
    let chunk_size = 16;
    let ptr_size = std::mem::size_of::<T::PtrSizeType>() * 2;

    let mut chunk_iter = section.data.slice().chunks(chunk_size);
    let mut prev;
    let mut curr = None;
    let mut next = chunk_iter.next();
    let mut first_skip = true;

    while let Some(chunk) = {
        prev = curr;
        curr = next;
        next = chunk_iter.next();
        if let Some(chunk) = prev {
            offset += chunk.len();
        }
        curr
    } {
        if prev == curr && curr == next {
            if first_skip {
                writeln!(f, "{:.>ptr_size$}", "")?;
                first_skip = false;
            }
            continue;
        }
        first_skip = true;

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
    }

    writeln!(f)?;
    Ok(())
}

impl<T: PrimInt + LowerHex> Symbol<T> {
    fn display<M: TranslationUnitMachine<PtrSizeType = T>>(
        &self,
        f: &mut impl std::fmt::Write,
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

        write!(
            f,
            "{offset:0>int_size$x} {size: >int_size$x} {kind:10} {visibility:6} "
        )?;
        if let Some(section_idx) = section {
            let section = trans.get(*section_idx);
            let section_name = section.name();
            let section_name = trans.str_table.get(section_name).unwrap_or_default();
            let section_name = "\"".to_owned() + section_name + "\"";
            write!(f, "{section_name: <10} \"{name}\"",)?;
        } else {
            write!(f, "None       \"{name}\"")?;
        }
        if let Some(dbg) = trans.symbols.get_symbol_dbg(idx) {
            if let Some(def) = &dbg.definition {
                let src = def.src_slice();
                write!(f, "\n{FAINT}   ^: def:  {def} -> {src:?}{RESET}",)?;
            }
            if let Some(def) = &dbg.size {
                let src = def.src_slice();
                write!(f, "\n{FAINT}   ^: size: {def} -> {src:?}{RESET}")?;
            }
            if let Some(def) = &dbg.ty {
                let src = def.src_slice();
                write!(f, "\n{FAINT}   ^: type: {def} -> {src:?}{RESET}")?;
            }
            if let Some(def) = &dbg.visibility {
                let src = def.src_slice();
                write!(f, "\n{FAINT}   ^: vis:  {def} -> {src:?}{RESET}")?;
            }
        }
        Ok(())
    }
}

impl<T: PrimInt + LowerHex> Symbols<T> {
    pub fn display<M: TranslationUnitMachine<PtrSizeType = T>>(
        &self,
        f: &mut impl std::fmt::Write,
        trans: &TranslationUnit<M>,
    ) -> std::fmt::Result {
        writeln!(f, "symbol table")?;
        let int_size = std::mem::size_of::<T>() * 2;
        let offset = "offset";
        let size = "size";
        let kind = "type";
        let visibility = "vis";
        let section = "section";
        let name = "name";

        indexed_list_display(
            trans.symbols.symbols(),
            f,
            std::format_args!(
                "{offset: >int_size$} {size: >int_size$} {kind:10} {visibility:6} {section:10} {name}"
            ),
            |(i, s), f| s.display(f, i, trans),
        )?;
        Ok(())
    }
}

pub fn indexed_list_display<T, F: std::fmt::Write>(
    list: impl ExactSizeIterator<Item = T>,
    f: &mut F,
    header: impl std::fmt::Display,
    mut disp: impl FnMut(T, &mut Indent<&mut F>) -> std::fmt::Result,
) -> std::fmt::Result {
    let max_int_length = list
        .len()
        .checked_ilog10()
        .map(|v| v as usize + 1)
        .unwrap_or(0);

    writeln!(f, "{: >max_int_length$}  {header}", "")?;

    for (idx, item) in list.enumerate() {
        write!(f, "{: >max_int_length$}: ", idx)?;
        disp(item, &mut Indent::new_middle(f, max_int_length))?;
        writeln!(f)?;
    }
    Ok(())
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

pub struct RightPad<O: Write> {
    out: O,
    pad: usize,
    current: usize,
}

impl<O: Write> RightPad<O> {
    pub fn new(out: O, pad: usize) -> Self {
        Self {
            out,
            pad,
            current: 0,
        }
    }
}

impl<O: Write> std::fmt::Write for RightPad<O> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.current += s.chars().count();
        self.out.write_str(s)
    }
}

impl<O: Write> Drop for RightPad<O> {
    fn drop(&mut self) {
        let pad = self.pad.saturating_sub(self.current);
        write!(self.out, "{: <pad$}", "").unwrap();
    }
}
