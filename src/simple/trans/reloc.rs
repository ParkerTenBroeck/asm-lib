use std::num::NonZeroUsize;

pub trait Reloc: Clone + std::fmt::Debug {}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct RelocIdx(NonZeroUsize);

#[derive(Clone, Debug)]
pub struct Relocations<T: Reloc> {
    relocs: Vec<T>,
}

impl<T: Reloc> Default for Relocations<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Reloc> Relocations<T> {
    pub fn new() -> Self {
        Self { relocs: Vec::new() }
    }

    pub fn get_mut(&mut self, index: RelocIdx) -> &mut T {
        &mut self.relocs[index.0.get() - 1]
    }

    pub fn get(&mut self, index: RelocIdx) -> &T {
        &self.relocs[index.0.get() - 1]
    }

    pub fn emit(&mut self, reloc: T) -> RelocIdx {
        self.relocs.push(reloc);
        RelocIdx(NonZeroUsize::new(self.relocs.len()).unwrap())
    }
    
    pub fn fmt<M: super::TranslationUnitMachine<Reloc = T>>(&self, f: &mut std::fmt::Formatter<'_>, trans: &super::TranslationUnit<M>) -> std::fmt::Result {
        
        Ok(())
    }
}
