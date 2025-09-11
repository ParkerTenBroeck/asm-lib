use std::collections::HashMap;

use num_traits::PrimInt;

use crate::{node::NodeOwned, simple::trans::reloc::RelocIdx};

#[derive(Clone, Debug)]
pub struct DataDbg<T: PrimInt> {
    pub range: std::ops::Range<T>,
    pub node: NodeOwned,
}

#[derive(Clone, Debug, Default)]
pub struct DebugInfo<T: PrimInt> {
    data: Vec<DataDbg<T>>,
    relocs: HashMap<RelocIdx, NodeOwned>,
    comments: Vec<(T, String)>,
}

impl<T: PrimInt> DebugInfo<T> {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            relocs: HashMap::new(),
            comments: Vec::new(),
        }
    }

    pub fn emit_reloc_dbg(&mut self, reloc_idx: RelocIdx, node: NodeOwned) {
        self.relocs.insert(reloc_idx, node);
    }

    pub fn emit_data_dbg(&mut self, range: std::ops::Range<T>, node: NodeOwned) {
        let index = self
            .data
            .partition_point(|data| data.range.end <= range.start);
        if let Some(last) = self.data.get_mut(index.wrapping_sub(1))
            && last.range.end == range.start
            && last.node == node
        {
            last.range.end = range.end;
        } else {
            self.data.insert(index, DataDbg { range, node })
        }
    }

    pub fn emit_comment_dbg(&mut self, offset: T, comment: &str) {
        let index = self.comments.partition_point(|(o, _)| *o <= offset);
        self.comments.insert(index, (offset, comment.to_owned()));
    }

    pub fn resolve_reloc_dbg(&self, reloc_idx: RelocIdx) -> Option<&NodeOwned> {
        self.relocs.get(&reloc_idx)
    }

    pub fn resolve_data_dbg(&self, range: impl std::ops::RangeBounds<T>) -> &[DataDbg<T>] {
        let start_idx = self.data.partition_point(|dbg| match range.start_bound() {
            std::ops::Bound::Included(v) => *v < dbg.range.start,
            std::ops::Bound::Excluded(v) => *v <= dbg.range.start,
            std::ops::Bound::Unbounded => false,
        });

        let end_idx = self.data.partition_point(|dbg| match range.end_bound() {
            std::ops::Bound::Included(v) => *v <= dbg.range.end,
            std::ops::Bound::Excluded(v) => *v < dbg.range.end,
            std::ops::Bound::Unbounded => true,
        });
        &self.data[start_idx..end_idx]
    }

    pub fn resolve_comments(&self, range: impl std::ops::RangeBounds<T>) -> &[(T, String)] {
        let start_idx = self
            .comments
            .partition_point(|(off, _)| match range.start_bound() {
                std::ops::Bound::Included(v) => *off < *v,
                std::ops::Bound::Excluded(v) => *off <= *v,
                std::ops::Bound::Unbounded => false,
            });

        let end_idx = self
            .comments
            .partition_point(|(off, _)| match range.end_bound() {
                std::ops::Bound::Included(v) => *off <= *v,
                std::ops::Bound::Excluded(v) => *off < *v,
                std::ops::Bound::Unbounded => true,
            });
        &self.comments[start_idx..end_idx]
    }
}
