use crate::lex::Span;
use std::path::Path;

pub trait Source {
    fn path(&self) -> &Path;
    fn contents(&self) -> &str;
    fn eof(&self) -> Span;
}

pub trait NodeTrait: Sized + Clone {
    fn span(&self) -> Span;
    fn source(&self) -> &impl Source;
    fn parent(&self) -> &Parent<Self>;
    fn top(mut self: &Self) -> &Self {
        while let Some(next) = self.parent().parent() {
            self = next;
        }
        self
    }
}

impl<'a> std::fmt::Display for NodeInfoRef<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let top = self.top();
        write!(
            f,
            "{}:{}:{}",
            top.source().path().display(),
            top.span().line as usize + 1,
            top.span().col as usize + 1,
        )
    }
}

impl std::fmt::Display for NodeInfoOwned {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let top = self.parent.parent().map(|p| &**p.top()).unwrap_or(self);
        write!(
            f,
            "{}:{}:{}",
            top.source.path().display(),
            top.span.line as usize + 1,
            top.span.col as usize + 1,
        )
    }
}

impl<'a> NodeTrait for NodeRef<'a> {
    fn span(&self) -> Span {
        self.span
    }

    fn parent(&self) -> &Parent<Self> {
        &self.parent
    }

    fn source(&self) -> &impl Source {
        self.source
    }
}

impl NodeTrait for NodeOwned {
    fn parent(&self) -> &Parent<Self> {
        &self.parent
    }

    fn source(&self) -> &impl Source {
        &self.source
    }

    fn span(&self) -> Span {
        self.span
    }
}

impl<'a> Source for SourceInfoRef<'a> {
    fn contents(&self) -> &str {
        self.contents
    }

    fn eof(&self) -> Span {
        self.eof()
    }

    fn path(&self) -> &Path {
        self.path
    }
}

impl Source for SourceOwned {
    fn contents(&self) -> &str {
        &self.contents
    }

    fn eof(&self) -> Span {
        self.eof()
    }

    fn path(&self) -> &Path {
        &self.path
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Parent<T> {
    None,
    Included { parent: T },
    Pasted { parent: T, definition: Option<T> },
    Generated { parent: T, definition: Option<T> },
}

impl<T> Parent<T> {
    pub fn parent(&self) -> Option<&T> {
        match self {
            Self::None => None,
            Self::Included { parent } => Some(parent),
            Self::Pasted { parent, .. } => Some(parent),
            Self::Generated { parent, .. } => Some(parent),
        }
    }
}

// ----------------------------------------

pub type NodeRef<'a> = &'a NodeInfoRef<'a>;
pub type SourceRef<'a> = &'a SourceInfoRef<'a>;

#[derive(Clone, Copy)]
pub struct SourceInfoRef<'a> {
    pub path: &'a Path,
    pub contents: &'a str,
}

impl<'a> SourceInfoRef<'a> {
    pub fn eof(&self) -> Span {
        Span {
            line: self.contents.lines().count() as u32,
            col: self.contents.lines().last().unwrap_or("").len() as u32,
            offset: (self.contents.len() as u32),
            len: 1,
        }
    }
}

impl<'a> PartialEq for SourceInfoRef<'a> {
    fn eq(&self, other: &Self) -> bool {
        if std::ptr::eq(self, other) {
            return true;
        };
        self.path == other.path
    }
}

impl<'a> std::fmt::Debug for SourceInfoRef<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Source").field("path", &self.path).finish()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Node<'a, T>(pub T, pub NodeRef<'a>);

impl<'a, T> Node<'a, T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Node<'a, U> {
        Node(f(self.0), self.1)
    }

    pub fn as_ref(&self) -> Node<'a, &T> {
        Node(&self.0, self.1)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct NodeInfoRef<'a> {
    pub span: Span,
    pub source: SourceRef<'a>,
    pub parent: Parent<NodeRef<'a>>,
}
impl<'a> NodeInfoRef<'a> {
    pub fn src_slice(&self) -> &'a str {
        &self.source.contents
            [self.span.offset as usize..self.span.offset as usize + self.span.len as usize]
    }
}

//-------------------------------
use std::sync::Arc;

pub type NodeOwned = Arc<NodeInfoOwned>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SourceOwned {
    pub path: Arc<Path>,
    pub contents: Arc<str>,
}

impl SourceOwned {
    pub fn eof(&self) -> Span {
        Span {
            line: self.contents.lines().count() as u32,
            col: self.contents.lines().last().unwrap_or("").len() as u32,
            offset: (self.contents.len() as u32),
            len: 1,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct NodeInfoOwned {
    pub span: Span,
    pub source: SourceOwned,
    pub parent: Parent<NodeOwned>,
}

impl NodeInfoOwned {
    pub fn src_slice(&self) -> &str {
        &self.source.contents
            [self.span.offset as usize..self.span.offset as usize + self.span.len as usize]
    }
}

pub struct NodeManager {
    // owned_source_map: HashMap<*const SourceInfoRef<'a>, SourceOwned>,
    // owned_node_map: HashMap<*const NodeInfoRef<'a>, NodeOwned>,
}

impl NodeManager {
    // pub fn merge_nodes<'a>(&self, left: NodeRef<'a>, right: NodeRef<'a>) -> NodeRef<'a> {
    //     // TODO optimizing this might be something to do
    //     fn meow<'a>(thingies: &mut Vec<NodeRef<'a>>, start: NodeRef<'a>) {
    //         let mut nya = Some(&start);
    //         while let Some(thing) = nya {
    //             thingies.push(thing);
    //             nya = thing.parent.parent();
    //         }
    //     }
    //     let mut lhs = Vec::new();
    //     let mut rhs = Vec::new();
    //     meow(&mut lhs, left);
    //     meow(&mut rhs, right);

    //     for (lhs, rhs) in lhs.iter().rev().zip(rhs.iter().rev()) {
    //         if lhs != rhs {
    //             if lhs.source != rhs.source {
    //                 panic!("uhhhhhhh, {left:?}, {right:?}")
    //             } else {
    //                 return self.node(NodeInfoRef {
    //                     span: lhs.span.combine(rhs.span),
    //                     source: lhs.source,
    //                     parent: lhs.parent,
    //                 });
    //             }
    //         }
    //     }
    //     left
    // }
}
