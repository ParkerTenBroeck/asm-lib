use crate::simple::trans::{
    TranslationUnitMachine, data::Data, dbg::DebugInfo, reloc::Relocations, str::StrIdx,
    sym::SymbolIdx,
};

pub struct Section<T: TranslationUnitMachine + ?Sized> {
    name: StrIdx,
    pub readable: bool,
    pub executable: bool,
    pub writeable: bool,
    pub offset: T::PtrSizeType,
    pub(crate) data: Data<T::PtrSizeType>,
    pub(crate) relocations: Relocations<T>,
    pub(crate) debug_info: DebugInfo<T::PtrSizeType>,
    symbols: Vec<(T::PtrSizeType, SymbolIdx)>,
}

impl<T: TranslationUnitMachine> std::fmt::Debug for Section<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Section")
            .field("name", &self.name)
            .field("data", &self.data)
            .field("offset", &self.offset)
            .field("relocations", &self.relocations)
            .field("debug_info", &self.debug_info)
            .field("readable", &self.readable)
            .field("executable", &self.executable)
            .field("writeable", &self.writeable)
            .field("symbols", &self.symbols)
            .finish()
    }
}

impl<T: TranslationUnitMachine + ?Sized> Clone for Section<T> {
    fn clone(&self) -> Self {
        Self {
            name: self.name,
            data: self.data.clone(),
            offset: self.offset,
            relocations: self.relocations.clone(),
            debug_info: self.debug_info.clone(),
            readable: self.readable,
            executable: self.executable,
            writeable: self.writeable,
            symbols: self.symbols.clone(),
        }
    }
}

impl<T: TranslationUnitMachine + ?Sized> Section<T> {
    pub fn new(name: StrIdx) -> Self {
        Self {
            name,
            data: Data::new(),
            offset: num_traits::zero(),
            relocations: Relocations::new(),
            debug_info: DebugInfo::new(),
            symbols: Vec::new(),
            readable: true,
            executable: false,
            writeable: false,
        }
    }

    pub fn name(&self) -> StrIdx {
        self.name
    }

    pub fn data(&self) -> &Data<T::PtrSizeType> {
        &self.data
    }

    pub fn relocations(&self) -> &Relocations<T> {
        &self.relocations
    }

    pub fn debug_info(&self) -> &DebugInfo<T::PtrSizeType> {
        &self.debug_info
    }

    pub fn get_symbols(
        &self,
        range: impl std::ops::RangeBounds<T::PtrSizeType>,
    ) -> &[(T::PtrSizeType, SymbolIdx)] {
        let start_idx = self
            .symbols
            .partition_point(|(off, _)| match range.start_bound() {
                std::ops::Bound::Included(v) => *off < *v,
                std::ops::Bound::Excluded(v) => *off <= *v,
                std::ops::Bound::Unbounded => false,
            });

        let end_idx = self
            .symbols
            .partition_point(|(off, _)| match range.end_bound() {
                std::ops::Bound::Included(v) => *off <= *v,
                std::ops::Bound::Excluded(v) => *off < *v,
                std::ops::Bound::Unbounded => true,
            });
        &self.symbols[start_idx..end_idx]
    }
    
    pub(crate) fn bind_symbol(&mut self, section_idx: SymbolIdx, offset: T::PtrSizeType) {
        let (Ok(idx)|Err(idx)) = self.symbols.binary_search_by_key(&offset, |(o, _)|*o);
        self.symbols.insert(idx, (offset, section_idx));
    }
}
