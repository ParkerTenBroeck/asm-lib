use std::{fmt::Debug, ops::Add};

use crate::simple::trans::{TranslationUnit, TranslationUnitMachine, merge::Merger};

pub trait Reloc: Clone + std::fmt::Debug {
    type Machine: TranslationUnitMachine<Reloc = Self>;

    fn display(
        &self,
        f: &mut impl std::fmt::Write,
        trans: &TranslationUnit<Self::Machine>,
    ) -> std::fmt::Result;

    fn max_size(&self) -> <Self::Machine as TranslationUnitMachine>::PtrSizeType;
    fn current_size(&self) -> <Self::Machine as TranslationUnitMachine>::PtrSizeType;
    fn offset(&self, merger: &Merger<Self::Machine>, offset: RelocOffset) -> Self;
    fn resolve(&self, trans: &mut TranslationUnit<Self::Machine>);
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct RelocIdx(usize);
impl RelocIdx {
    pub fn offset(&self, reloc_offset: RelocOffset) -> RelocIdx {
        Self(self.0.add(reloc_offset.0))
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct RelocOffset(usize);

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

    pub(crate) fn offset(&self) -> RelocOffset {
        RelocOffset(self.relocs.len())
    }
}

impl<T: TranslationUnitMachine<PtrSizeType: Ord> + ?Sized> Relocations<T> {
    pub fn find_range(
        &self,
        range: impl std::ops::RangeBounds<T::PtrSizeType>,
    ) -> impl ExactSizeIterator<Item = (RelocIdx, T::PtrSizeType, &T::Reloc)> {
        let start_idx = self
            .relocs
            .partition_point(|(o, _)| match range.start_bound() {
                std::ops::Bound::Included(v) => o < v,
                std::ops::Bound::Excluded(v) => o <= v,
                std::ops::Bound::Unbounded => false,
            });

        let end_idx = self
            .relocs
            .partition_point(|(o, _)| match range.end_bound() {
                std::ops::Bound::Included(v) => o <= v,
                std::ops::Bound::Excluded(v) => o < v,
                std::ops::Bound::Unbounded => true,
            });
        self.relocs[start_idx..end_idx]
            .iter()
            .enumerate()
            .map(|(i, (p, r))| (RelocIdx(i), *p, r))
    }

    pub fn find_at(
        &self,
        offset: T::PtrSizeType,
    ) -> impl ExactSizeIterator<Item = (RelocIdx, T::PtrSizeType, &T::Reloc)> {
        self.find_range(offset..=offset)
    }
}
