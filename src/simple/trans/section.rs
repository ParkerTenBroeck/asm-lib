use crate::simple::trans::{
    TranslationUnitMachine, data::Data, dbg::DebugInfo, reloc::Relocations, str::StrIdx,
};

pub struct Section<T: TranslationUnitMachine> {
    name: StrIdx,
    pub(crate) data: Data<T::PtrSizeType>,
    pub(crate) relocations: Relocations<T::Reloc>,
    pub(crate) debug_info: DebugInfo,
}

impl<T: TranslationUnitMachine> std::fmt::Debug for Section<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Section")
            .field("name", &self.name)
            .field("data", &self.data)
            .field("relocations", &self.relocations)
            .field("debug_info", &self.debug_info)
            .finish()
    }
}

impl<T: TranslationUnitMachine> Clone for Section<T> {
    fn clone(&self) -> Self {
        Self {
            name: self.name,
            data: self.data.clone(),
            relocations: self.relocations.clone(),
            debug_info: self.debug_info.clone(),
        }
    }
}

impl<T: TranslationUnitMachine> Section<T> {
    pub fn new(name: StrIdx) -> Self {
        Self {
            name,
            data: Data::new(),
            relocations: Relocations::new(),
            debug_info: DebugInfo::new(),
        }
    }

    pub fn name(&self) -> StrIdx{
        self.name
    }
}
