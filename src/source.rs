use std::{collections::HashMap, error::Error, path::Path};

use bumpalo::Bump;

use crate::{SourceInfoRef, SourceRef};

pub type SourceSupplier<'a> =
    Box<dyn FnMut(&'a Path, &'a Bump) -> Result<SourceContents<'a>, Box<dyn Error>> + 'a>;

pub enum SourceContents<'a> {
    Text(&'a str),
    Bin(&'a [u8]),
}

#[derive(Clone, Copy, Debug)]
pub enum SourceInfo<'a> {
    Text(SourceRef<'a>),
    Bin(&'a Path, &'a [u8]),
}

pub struct Sources<'a> {
    supplier: SourceSupplier<'a>,
    source_map: HashMap<&'a Path, SourceInfo<'a>>,
}

impl<'a> Sources<'a> {
    pub fn new(supplier: SourceSupplier<'a>) -> Self {
        Self {
            supplier,
            source_map: HashMap::new(),
        }
    }

    pub fn get_src(
        &mut self,
        bump: &'a Bump,
        path: &'a Path,
    ) -> Result<SourceInfo<'a>, Box<dyn Error>> {
        if let Some(src) = self.source_map.get(path) {
            return Ok(*src);
        }

        let res = match (self.supplier)(path, bump)? {
            SourceContents::Text(contents) => {
                SourceInfo::Text(bump.alloc(SourceInfoRef { path, contents }))
            }
            SourceContents::Bin(items) => match std::str::from_utf8(items) {
                Ok(contents) => SourceInfo::Text(bump.alloc(SourceInfoRef { path, contents })),
                Err(_) => SourceInfo::Bin(path, items),
            },
        };

        self.source_map.insert(path, res);
        Ok(res)
    }

    pub fn get_text(
        &mut self,
        bump: &'a Bump,
        path: &'a Path,
    ) -> Result<SourceRef<'a>, Box<dyn Error>> {
        match self.get_src(bump, path)? {
            SourceInfo::Text(text) => Ok(text),
            SourceInfo::Bin(_, _) => Err("source text must be valid utf-8")?,
        }
    }

    pub fn get_bin(&mut self, bump: &'a Bump, path: &'a Path) -> Result<&'a [u8], Box<dyn Error>> {
        match self.get_src(bump, path)? {
            SourceInfo::Text(text) => Ok(text.contents.as_bytes()),
            SourceInfo::Bin(_, bin) => Ok(bin),
        }
    }
}
