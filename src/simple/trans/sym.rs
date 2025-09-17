use std::{collections::HashMap, fmt::Display};

use num_traits::PrimInt;

use crate::{
    logs::LogEntry,
    node::NodeOwned,
    simple::trans::{
        SectionIdx, TranslationUnitMachine,
        str::{StrIdx, StringTable},
    },
};

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct SymbolIdx(usize);

impl std::fmt::Display for SymbolIdx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (self.0 + 1).fmt(f)
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug, Default)]
pub enum SymbolType {
    #[default]
    Unresolved,
    Notype,
    Section,
    Object,
    File,
    Common,
    Function,
    Data,
}

impl Display for SymbolType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ident = match self {
            SymbolType::Unresolved => "unresolved",
            SymbolType::Notype => "notype",
            SymbolType::Section => "section",
            SymbolType::Object => "object",
            SymbolType::File => "file",
            SymbolType::Common => "common",
            SymbolType::Function => "function",
            SymbolType::Data => "data",
        };
        ident.fmt(f)
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug, Default)]
pub enum SymbolVisibility {
    #[default]
    Local,
    Weak,
    Global,
}

impl Display for SymbolVisibility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ident = match self {
            SymbolVisibility::Local => "local",
            SymbolVisibility::Global => "global",
            SymbolVisibility::Weak => "weak",
        };
        ident.fmt(f)
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct Symbol<T: PrimInt> {
    name: StrIdx,
    pub section: Option<SectionIdx>,
    pub ty: SymbolType,
    pub visibility: SymbolVisibility,
    pub size: T,
    pub offset: T,
}

impl<T: PrimInt> Symbol<T> {
    pub fn new(name: StrIdx) -> Self {
        Self {
            name,
            section: None,
            ty: Default::default(),
            visibility: Default::default(),
            size: num_traits::zero(),
            offset: num_traits::zero(),
        }
    }

    pub fn name(&self) -> StrIdx {
        self.name
    }

    pub fn merge<M: TranslationUnitMachine<PtrSizeType = T>>(
        &self,
        str_table: &StringTable,
        linker: &mut super::link::Linker<M>,
    ) -> Symbol<T> {
        let mut copy = *self;

        //TODO
        if let Some(section) = copy.section {
            copy.offset = copy.offset.saturating_add(linker.section_offset(section));
        }

        copy.name = linker
            .str_table()
            .resolve(str_table.get(copy.name).unwrap_or_default());

        copy
    }
}

#[derive(Clone, Debug, Default)]
pub struct Symbols<T: PrimInt> {
    symbols: Vec<Symbol<T>>,
    symbol_map: HashMap<StrIdx, SymbolIdx>,
    symbol_dbg_map: HashMap<SymbolIdx, SymbolDbg>,
}

pub enum SymbolError {
    SizePreviouslyDeclared(Option<NodeOwned>),
    KindPreviouslyDeclared(Option<NodeOwned>),
    VisibilityPreviouslyDeclared(Option<NodeOwned>),
    PreviouslyBound(Option<NodeOwned>),
}
impl SymbolError {
    pub fn to_log_entry(&self, label: &str, node: NodeOwned) -> LogEntry<NodeOwned> {
        let (log, node) = match self {
            SymbolError::SizePreviouslyDeclared(last_node) => (
                LogEntry::new().warning(node, format!("size previously defined for '{label}'")),
                last_node,
            ),
            SymbolError::KindPreviouslyDeclared(last_node) => (
                LogEntry::new().warning(node, format!("kind previously defined for '{label}'")),
                last_node,
            ),
            SymbolError::VisibilityPreviouslyDeclared(last_node) => (
                LogEntry::new()
                    .warning(node, format!("visibility previously defined for '{label}'")),
                last_node,
            ),
            SymbolError::PreviouslyBound(last_node) => (
                LogEntry::new().error(node, format!("symbol '{label}' previously bound")),
                last_node,
            ),
        };
        if let Some(node) = node.clone() {
            log.info(node, "here")
        } else {
            log
        }
    }
}

#[derive(Default, Clone, Debug)]
pub struct SymbolDbg {
    pub definition: Option<NodeOwned>,
    pub visibility: Option<NodeOwned>,
    pub ty: Option<NodeOwned>,
    pub size: Option<NodeOwned>,
}

impl<T: PrimInt> Symbols<T> {
    pub fn new() -> Self {
        Self {
            symbols: Vec::new(),
            symbol_map: HashMap::new(),
            symbol_dbg_map: HashMap::new(),
        }
    }

    pub fn resolve(&mut self, name: StrIdx) -> Option<SymbolIdx> {
        self.symbol_map.get(&name).copied()
    }

    pub fn resolve_or_make(&mut self, name: StrIdx) -> SymbolIdx {
        if let Some(symbol_idx) = self.symbol_map.get(&name).copied() {
            symbol_idx
        } else {
            let symbol_idx = SymbolIdx(self.symbols.len());
            self.symbols.push(Symbol::new(name));
            self.symbol_map.insert(name, symbol_idx);
            symbol_idx
        }
    }

    pub fn symbol_mut(&mut self, symbol_idx: SymbolIdx) -> Option<&mut Symbol<T>> {
        self.symbols.get_mut(symbol_idx.0)
    }

    pub fn symbol(&self, symbol_idx: SymbolIdx) -> Option<&Symbol<T>> {
        self.symbols.get(symbol_idx.0)
    }

    pub fn reset_locals(&mut self) {
        self.symbol_map.retain(|_, e| {
            self.symbols
                .get_mut(e.0)
                .map(|s| s.visibility)
                .unwrap_or(SymbolVisibility::Local)
                != SymbolVisibility::Local
        });
    }

    pub fn symbols(&self) -> impl ExactSizeIterator<Item = (SymbolIdx, &Symbol<T>)> {
        self.symbols
            .iter()
            .enumerate()
            .map(|(i, s)| (SymbolIdx(i), s))
    }

    pub fn len(&self) -> usize {
        self.symbols.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub(super) fn symbol_dbg_entry(
        &mut self,
        symbol_idx: SymbolIdx,
    ) -> std::collections::hash_map::Entry<'_, SymbolIdx, SymbolDbg> {
        self.symbol_dbg_map.entry(symbol_idx)
    }

    pub fn get_symbol_dbg(&self, symbol_idx: SymbolIdx) -> Option<&SymbolDbg> {
        self.symbol_dbg_map.get(&symbol_idx)
    }

    pub fn create_bound(&mut self, sym: Symbol<T>, dbg: Option<SymbolDbg>) -> SymbolIdx {
        let symbol_idx = SymbolIdx(self.symbols.len());
        self.symbols.push(sym);
        if let Some(dbg) = dbg {
            self.symbol_dbg_map.insert(symbol_idx, dbg);
        }
        symbol_idx
    }
}
