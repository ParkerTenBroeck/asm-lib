use std::collections::HashMap;

use num_traits::CheckedAdd;

use crate::{
    LogEntry, Logs, NodeOwned,
    simple::trans::{
        SectionIdx, TranslationUnit, TranslationUnitMachine,
        data::PushDataResult,
        reloc::{Reloc, RelocOffset},
        sym::{Symbol, SymbolDbg, SymbolIdx, SymbolType, SymbolVisibility},
    },
};

pub struct Merger<M: TranslationUnitMachine> {
    symbol_map: HashMap<SymbolIdx, SymbolIdx>,
    section_map: HashMap<SectionIdx, (SectionIdx, M::PtrSizeType, RelocOffset)>,
    logs: Logs<NodeOwned>,
}

impl<M: TranslationUnitMachine> Merger<M> {
    pub fn new() -> Self {
        Self {
            symbol_map: Default::default(),
            section_map: Default::default(),
            logs: Logs::new(),
        }
    }

    pub fn take_logs(&mut self) -> Logs<NodeOwned> {
        self.logs.take()
    }

    pub fn clear(&mut self) {
        self.symbol_map.clear();
        self.section_map.clear();
    }

    pub fn merge_many(&mut self, into: &mut TranslationUnit<M>, from: &[TranslationUnit<M>]) {
        for from in from {
            self.merge(into, from);
        }
    }

    pub fn merge(&mut self, into: &mut TranslationUnit<M>, from: &TranslationUnit<M>) {
        self.clear();
        into.reset_locals();
        for (from_section_idx, from_section) in from.sections() {
            let into_section =
                into.resolve_mut(from.get_str(from_section.name()).unwrap_or_default());

            let result = into_section
                .section
                .data
                .push_align(from_section.data().align());

            if let PushDataResult::NotEnoughSpace = result {
                self.logs.report_error_locless(format!(
                    "section '{}' exceeded maximum size",
                    from.str_table
                        .get(from_section.name())
                        .unwrap_or_default()
                        .escape_debug()
                ));
            }

            self.section_map.insert(
                from_section_idx,
                (
                    into_section.section_idx,
                    into_section.section.data().current_offset(),
                    into_section.section.relocations.offset(),
                ),
            );

            let result = into_section
                .section
                .data
                .push_data(from_section.data().slice(), from_section.data().align());

            if let PushDataResult::NotEnoughSpace = result {
                self.logs.report_error_locless(format!(
                    "section '{}' exceeded maximum size",
                    from.str_table
                        .get(from_section.name())
                        .unwrap_or_default()
                        .escape_debug()
                ));
            }
        }

        for (from_symbol_idx, from_symbol) in from.symbols.symbols() {
            self.merge_symbol(into, from_symbol_idx, from_symbol, from);
        }

        for (from_section_idx, (into_section_idx, section_offset, reloc_offset)) in
            self.section_map.iter()
        {
            let from_section = from.get(*from_section_idx);
            let into_section = into.get_mut(*into_section_idx);

            for (comment_offset, comment) in from_section.debug_info.resolve_comments(..) {
                match comment_offset.checked_add(section_offset) {
                    Some(offset) => {
                        into_section
                            .section
                            .debug_info
                            .emit_comment_dbg(offset, comment);
                    }
                    None => todo!(),
                }
            }
            for data_dbg in from_section.debug_info.resolve_data_dbg(..) {
                let start = match data_dbg.range.start.checked_add(section_offset) {
                    Some(start) => start,
                    None => {
                        self.logs.report_error(
                            data_dbg.node.clone(),
                            "data range overflowed while mergineging",
                        );
                        return;
                    }
                };

                let end = match data_dbg.range.end.checked_add(section_offset) {
                    Some(end) => end,
                    None => {
                        self.logs.report_error(
                            data_dbg.node.clone(),
                            "data range overflowed while mergineging",
                        );
                        return;
                    }
                };

                into_section
                    .section
                    .debug_info
                    .emit_data_dbg(start..end, data_dbg.node.clone());
            }

            for (reloc_idx, node) in from_section.debug_info.reloc_dbg() {
                into_section
                    .section
                    .debug_info
                    .emit_reloc_dbg(reloc_idx.offset(*reloc_offset), node.clone());
            }

            for (from_reloc_idx, offset, reloc) in from_section.relocations.find_range(..) {
                match offset.checked_add(section_offset) {
                    Some(offset) => {
                        into_section
                            .section
                            .relocations
                            .emit(offset, reloc.offset(self, *reloc_offset));
                    }
                    None => {
                        let mut str =
                            "relocation offset overflowed while mergineging: '".to_owned();
                        reloc.display(&mut str, from).unwrap();
                        str.push('\'');
                        if let Some(dbg) = from_section.debug_info.resolve_reloc_dbg(from_reloc_idx)
                        {
                            self.logs.report_error(dbg.clone(), str);
                        } else {
                            self.logs.report_error_locless(str);
                        }
                        return;
                    }
                }
            }
        }
    }

    fn merge_symbol(
        &mut self,
        into: &mut TranslationUnit<M>,
        from_symbol_idx: SymbolIdx,
        from_symbol: &Symbol<M::PtrSizeType>,
        from: &TranslationUnit<M>,
    ) {
        let dbg = from.symbols.get_symbol_dbg(from_symbol_idx).cloned();

        let name_str = from.str_table.get(from_symbol.name()).unwrap_or_default();
        let name_idx = into.str_table.resolve(name_str);
        let mut to_symbol = from_symbol.with_name(name_idx);
        if let Some(section) = to_symbol.section {
            let (new_section_idx, offset, _) = self
                .section_map
                .get(&section)
                .expect("expected section mapping to exist");
            to_symbol.offset = match to_symbol.offset.checked_add(offset) {
                Some(ok) => ok,
                None => {
                    if let Some(node) = from
                        .symbols
                        .get_symbol_dbg(from_symbol_idx)
                        .and_then(SymbolDbg::location_best_effort)
                        .cloned()
                    {
                        self.logs.report_error(
                            node,
                            format!(
                                "symbol offset overflowed while mergineging: '{}'",
                                name_str.escape_debug()
                            ),
                        );
                    } else {
                        self.logs.report_error_locless(format!(
                            "symbol offset overflowed while mergineging: '{}'",
                            name_str.escape_debug()
                        ));
                    }
                    return;
                }
            };
            to_symbol.section = Some(*new_section_idx)
        }

        if to_symbol.ty == SymbolType::Unresolved {
            self.symbol_map.insert(
                from_symbol_idx,
                into.symbols.resolve_or_make(to_symbol.name()),
            );
            return;
        }

        if to_symbol.visibility == SymbolVisibility::Local {
            self.symbol_map
                .insert(from_symbol_idx, into.symbols.create_bound(to_symbol, dbg));
            return;
        }

        let Some(existing_symbol_idx) = into.symbols.resolve(to_symbol.name()) else {
            self.symbol_map
                .insert(from_symbol_idx, into.symbols.create_bound(to_symbol, dbg));
            return;
        };

        let existing_symbol = into
            .symbols
            .symbol_mut(existing_symbol_idx)
            .expect("expected symbol from index");

        if existing_symbol.ty == SymbolType::Unresolved
            || existing_symbol.visibility == SymbolVisibility::Weak
        {
            *existing_symbol = to_symbol;
            into.symbols.set_symbol_dbg(
                existing_symbol_idx,
                from.symbols.get_symbol_dbg(from_symbol_idx).cloned(),
            );
            self.symbol_map.insert(from_symbol_idx, existing_symbol_idx);
            return;
        }

        if existing_symbol.visibility == SymbolVisibility::Local {
            self.symbol_map
                .insert(from_symbol_idx, into.symbols.create_bound(to_symbol, dbg));
            return;
        }

        if let Some(node) = from
            .symbols
            .get_symbol_dbg(from_symbol_idx)
            .and_then(SymbolDbg::location_best_effort)
            .cloned()
        {
            if let Some(first_node) = into
                .symbols
                .get_symbol_dbg(existing_symbol_idx)
                .and_then(SymbolDbg::location_best_effort)
                .cloned()
            {
                self.logs.report(
                    LogEntry::new()
                        .error(
                            node,
                            format!("symbol '{}' previously bound", name_str.escape_debug()),
                        )
                        .info(first_node, "here"),
                )
            } else {
                self.logs.report_error(
                    node,
                    format!("symbol '{}' previously bound", name_str.escape_debug()),
                );
            }
        } else {
            self.logs.report_error_locless(format!(
                "symbol '{}' previously bound",
                name_str.escape_debug()
            ));
        }
    }

    pub fn resolve_symbol(&self, symbol_idx: SymbolIdx) -> SymbolIdx {
        *self
            .symbol_map
            .get(&symbol_idx)
            .expect("expected symbol map to exist")
    }
}

impl<M: TranslationUnitMachine> Default for Merger<M> {
    fn default() -> Self {
        Self::new()
    }
}
