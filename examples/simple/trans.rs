use assembler::simple::trans::{TranslationUnitMachine, reloc::Reloc};

pub struct MipsTranslationUnit;

impl TranslationUnitMachine for MipsTranslationUnit {
    type Reloc = MipsReloc;
    type PtrSizeType = u32;
}

#[derive(Debug, Clone)]
pub enum MipsReloc {}

impl Reloc for MipsReloc {}
