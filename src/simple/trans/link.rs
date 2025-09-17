use std::collections::HashMap;

use crate::simple::trans::{
    SectionIdx, TranslationUnit, TranslationUnitMachine,
    str::StringTable,
    sym::{Symbol, SymbolIdx, SymbolType, SymbolVisibility},
};

pub struct LinkerResult<M: TranslationUnitMachine> {
    pub result: TranslationUnit<M>,
}

pub struct Linker<M: TranslationUnitMachine> {
    symbol_map: HashMap<SymbolIdx, SymbolIdx>,
    section_map: HashMap<SectionIdx, SectionIdx>,
    section_data_offset: HashMap<SectionIdx, M::PtrSizeType>,
    trans: TranslationUnit<M>,
}

impl<M: TranslationUnitMachine> Linker<M> {
    pub fn link(&mut self, parts: &[TranslationUnit<M>]) -> LinkerResult<M> {
        let Some((first, remainder)) = parts.split_first() else {
            return LinkerResult {
                result: Default::default(),
            };
        };
        self.trans = first.clone();
        if !remainder.is_empty() {
            self.merge(remainder)
        }

        self.resolve();

        //TODO
        LinkerResult {
            result: self.trans.clone(),
        }
    }

    pub fn str_table(&mut self) -> &mut StringTable {
        &mut self.trans.str_table
    }

    pub fn merge(&mut self, from: &[TranslationUnit<M>]) {
        for from in from {
            self.section_data_offset.clear();
            self.symbol_map.clear();
            self.section_map.clear();

            for (from_section_idx, from_section) in from.sections() {
                let into_section = self
                    .trans
                    .resolve_mut(from.get_str(from_section.name()).unwrap_or_default());
                into_section
                    .section
                    .data
                    .push_align(from_section.data().align());
                self.section_data_offset.insert(
                    into_section.section_idx,
                    into_section.section.data().current_offset(),
                );
                self.section_map
                    .insert(from_section_idx, into_section.section_idx);
                into_section
                    .section
                    .data
                    .push_data(from_section.data().slice(), from_section.data().align());
            }

            for (from_symbol_idx, from_symbol) in from.symbols.symbols() {
                self.merge_symbol(from_symbol_idx, from_symbol, from);
            }

            for (from_section_idx, from_section) in from.sections() {
                for (reloc_idx, offset, reloc) in from_section.relocations.find_range(..) {
                    self.trans.get_mut(from_section_idx);
                    // into_section.section.relocations.emit(offset, reloc)
                }
            }
        }
        self.section_data_offset.clear();
        self.symbol_map.clear();
        self.symbol_map.clear();
    }

    fn merge_symbol(
        &mut self,
        from_symbol_idx: SymbolIdx,
        from_symbol: &Symbol<M::PtrSizeType>,
        from: &TranslationUnit<M>,
    ) {
        let dbg = from.symbols.get_symbol_dbg(from_symbol_idx).cloned();
        let to_symbol = from_symbol.merge(&from.str_table, self);

        if to_symbol.ty == SymbolType::Unresolved {
            self.symbol_map.insert(
                from_symbol_idx,
                self.trans.symbols.resolve_or_make(to_symbol.name()),
            );
            return;
        }

        match to_symbol.visibility {
            SymbolVisibility::Local => {
                self.symbol_map.insert(
                    from_symbol_idx,
                    self.trans.symbols.create_bound(to_symbol, dbg),
                );
            }
            SymbolVisibility::Weak => {
                let existing_symbol_idx = self.trans.symbols.resolve(to_symbol.name());
                if let Some(existing_symbol_idx) = existing_symbol_idx {
                    let visibility = self
                        .trans
                        .symbols
                        .symbol(existing_symbol_idx)
                        .expect("expected symbol from index")
                        .visibility;

                    match visibility {
                        SymbolVisibility::Weak => {
                            // do nothing
                        }
                        SymbolVisibility::Global => {
                            self.symbol_map.insert(from_symbol_idx, existing_symbol_idx);
                        }
                        SymbolVisibility::Local => panic!(),
                    }
                } else {
                    self.symbol_map.insert(
                        from_symbol_idx,
                        self.trans.symbols.create_bound(to_symbol, dbg),
                    );
                }
            }
            SymbolVisibility::Global => {
                let existing_symbol_idx = self.trans.symbols.resolve(to_symbol.name());
                if let Some(existing_symbol_idx) = existing_symbol_idx {
                    let symbol = self
                        .trans
                        .symbols
                        .symbol_mut(existing_symbol_idx)
                        .expect("expected symbol from index");

                    match symbol.visibility {
                        SymbolVisibility::Weak => {
                            *symbol = to_symbol;
                            self.symbol_map.insert(from_symbol_idx, existing_symbol_idx);
                        }
                        SymbolVisibility::Global => panic!(),
                        SymbolVisibility::Local => panic!(),
                    }
                } else {
                    self.symbol_map.insert(
                        from_symbol_idx,
                        self.trans.symbols.create_bound(to_symbol, dbg),
                    );
                }
            }
        }
    }

    pub fn resolve(&mut self) {}

    pub fn section_offset(&self, section: SectionIdx) -> M::PtrSizeType {
        self.section_data_offset
            .get(&section)
            .copied()
            .unwrap_or(num_traits::zero())
    }
}
