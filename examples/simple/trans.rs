use assembler::simple::trans::{TranslationUnitMachine, reloc::Reloc};

pub struct MipsTranslationUnit;

impl TranslationUnitMachine for MipsTranslationUnit {
    type Reloc = MipsReloc;
    type PtrSizeType = u32;
}

#[derive(Debug, Clone)]
pub struct MipsReloc {}

impl Reloc for MipsReloc {}
