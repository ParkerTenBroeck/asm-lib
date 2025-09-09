use std::{fmt::Debug, num::NonZeroUsize};

use crate::simple::trans::TranslationUnitMachine;

pub trait Reloc: Clone + std::fmt::Debug {}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct RelocIdx(NonZeroUsize);

pub struct Relocations<T: TranslationUnitMachine + ?Sized> {
    relocs: Vec<(T::PtrSizeType, T::Reloc)>,
}

impl<T: TranslationUnitMachine + ?Sized> Clone for Relocations<T> {
    fn clone(&self) -> Self {
        Self {
            relocs: self.relocs.clone(),
        }
    }
}

impl<T: TranslationUnitMachine + ?Sized> Debug for Relocations<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Relocations")
            .field("relocs", &self.relocs)
            .finish()
    }
}

impl<T: TranslationUnitMachine + ?Sized> Default for Relocations<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: TranslationUnitMachine + ?Sized> Relocations<T> {
    pub fn new() -> Self {
        Self { relocs: Vec::new() }
    }

    pub fn get_mut(&mut self, index: RelocIdx) -> &mut (T::PtrSizeType, T::Reloc) {
        &mut self.relocs[index.0.get() - 1]
    }

    pub fn get(&mut self, index: RelocIdx) -> &(T::PtrSizeType, T::Reloc) {
        &self.relocs[index.0.get() - 1]
    }

    pub fn emit(&mut self, offset: T::PtrSizeType, reloc: T::Reloc) -> RelocIdx {
        self.relocs.push((offset, reloc));
        RelocIdx(NonZeroUsize::new(self.relocs.len()).unwrap())
    }
}

impl<T: TranslationUnitMachine<PtrSizeType: Ord> + ?Sized> Relocations<T> {
    pub fn find_at(
        &self,
        offset: T::PtrSizeType,
    ) -> impl Iterator<Item = &(T::PtrSizeType, T::Reloc)> {
        match self.relocs.binary_search_by_key(&offset, |(v, _)| *v) {
            Ok(ok) => {
                let mut start = ok;
                for (o, _) in self.relocs[..start].iter().rev() {
                    if *o != offset {
                        break;
                    }
                    start -= 1;
                }
                let mut end = ok;
                for (o, _) in self.relocs[end..].iter() {
                    if *o != offset {
                        break;
                    }
                    end += 1;
                }
                self.relocs[start..end].iter()
            }
            Err(_) => [].iter(),
        }
    }
}
