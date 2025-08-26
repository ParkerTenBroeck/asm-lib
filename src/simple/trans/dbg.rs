use std::collections::{HashMap};

use crate::{
    node::NodeOwned,
    simple::trans::{reloc::RelocIdx},
};

#[derive(Clone, Debug)]
pub struct DataDbg {
    range: std::ops::Range<usize>,
    node: NodeOwned,
}

#[derive(Clone, Debug, Default)]
pub struct DebugInfo {
    data: Vec<DataDbg>,
    relocs: HashMap<RelocIdx, NodeOwned>,
}

impl DebugInfo {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            relocs: HashMap::new(),
        }
    }

    pub(super) fn emit_reloc_dbg(&mut self, reloc_idx: RelocIdx, node: NodeOwned) {
        self.relocs.insert(reloc_idx, node);
    }

    /// range must start at greater address than all ranges currently present
    pub(super) fn emit_data_dbg(&mut self, range: std::ops::Range<usize>, node: NodeOwned) {
        if let Some(last) = self.data.last_mut()
            && last.range.end == range.start
            && last.node == node
        {
            last.range.end = range.end;
        } else {
            self.data.push(DataDbg { range, node })
        }
    }
}
