use crate::simple::trans::{
    TranslationUnitMachine, data::Data, dbg::DebugInfo, reloc::Relocations, str::StrIdx,
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
}
