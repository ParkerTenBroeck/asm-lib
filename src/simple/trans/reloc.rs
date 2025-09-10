use std::fmt::Debug;

use crate::simple::trans::{TranslationUnit, TranslationUnitMachine};

pub trait Reloc: Clone + std::fmt::Debug {
    type Machine: TranslationUnitMachine<Reloc = Self>;

    fn display(
        &self,
        f: &mut impl std::fmt::Write,
        trans: &TranslationUnit<Self::Machine>,
    ) -> std::fmt::Result;
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct RelocIdx(usize);

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

    pub fn relocs(&self) -> impl ExactSizeIterator<Item = (RelocIdx, T::PtrSizeType, &T::Reloc)> {
        self.relocs
            .iter()
            .enumerate()
            .map(|(i, (o, r))| (RelocIdx(i), *o, r))
    }

    pub fn get_mut(&mut self, index: RelocIdx) -> &mut (T::PtrSizeType, T::Reloc) {
        &mut self.relocs[index.0]
    }

    pub fn get(&mut self, index: RelocIdx) -> &(T::PtrSizeType, T::Reloc) {
        &self.relocs[index.0]
    }

    pub fn emit(&mut self, offset: T::PtrSizeType, reloc: T::Reloc) -> RelocIdx {
        self.relocs.push((offset, reloc));
        RelocIdx(self.relocs.len() - 1)
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
