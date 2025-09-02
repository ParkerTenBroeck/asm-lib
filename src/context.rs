use super::logs::LogEntry;
use crate::Logs;
use crate::config::AssemblerConfig;
use crate::lex::Span;
use crate::lex::Token;
use crate::logs::LogPart;
use crate::source::SourceInfo;
use crate::source::Sources;
use bumpalo::Bump;
use std::fmt::Display;
use std::path::Path;
use std::sync::Arc;
use std::{collections::HashMap, error::Error};

pub use super::node::*;

pub type SourceSupplier<'a> =
    Box<dyn Fn(&'a Path, &Context<'a>) -> Result<&'a str, Box<dyn Error>> + 'a>;

pub struct Context<'a> {
    bump: &'a Bump,

    sources: Sources<'a>,
    top_src: SourceRef<'a>,
    top_src_eof: NodeRef<'a>,

    logs: Logs<NodeOwned>,

    config: AssemblerConfig,

    owned_source_map: HashMap<*const SourceInfoRef<'a>, SourceOwned>,
    owned_node_map: HashMap<*const NodeInfoRef<'a>, NodeOwned>,
}

impl<'a> Context<'a> {
    pub fn new(
        path: &'a Path,
        bump: &'a Bump,
        config: AssemblerConfig,
        mut sources: Sources<'a>,
    ) -> Self {
        let mut logs = Logs::new();

        let (top_src, top_src_eof) = match sources.get_text(bump, path) {
            Ok(source) => (
                source,
                &*bump.alloc(NodeInfoRef {
                    span: Span::empty(),
                    source,
                    parent: Parent::None,
                }),
            ),
            Err(err) => {
                logs.report_error_locless(format!(
                    "cannot assemble file '{}': {err}",
                    path.display()
                ));

                let source = bump.alloc(SourceInfoRef { path, contents: "" });
                (
                    &*source,
                    &*bump.alloc(NodeInfoRef {
                        span: Span::empty(),
                        source,
                        parent: Parent::None,
                    }),
                )
            }
        };

        Self {
            bump,
            sources,
            logs,
            top_src,
            top_src_eof,
            config,

            owned_source_map: HashMap::new(),
            owned_node_map: HashMap::new(),
        }
    }

    pub fn config(&self) -> &AssemblerConfig {
        &self.config
    }

    pub fn get_text(&mut self, path: &'a Path) -> Result<&'a SourceInfoRef<'a>, Box<dyn Error>> {
        self.sources.get_text(self.bump, path)
    }

    pub fn get_src(&mut self, path: &'a Path) -> Result<SourceInfo<'a>, Box<dyn Error>> {
        self.sources.get_src(self.bump, path)
    }

    pub fn get_bin(&mut self, path: &'a Path) -> Result<&'a [u8], Box<dyn Error>> {
        self.sources.get_bin(self.bump, path)
    }

    pub fn merge_nodes(&self, left: NodeRef<'a>, right: NodeRef<'a>) -> NodeRef<'a> {
        // TODO optimizing this might be something to do
        fn meow<'a>(thingies: &mut Vec<NodeRef<'a>>, start: NodeRef<'a>) {
            let mut nya = Some(&start);
            while let Some(thing) = nya {
                thingies.push(thing);
                nya = thing.parent.parent();
            }
        }
        let mut lhs = Vec::new();
        let mut rhs = Vec::new();
        meow(&mut lhs, left);
        meow(&mut rhs, right);

        for (lhs, rhs) in lhs.iter().rev().zip(rhs.iter().rev()) {
            if lhs != rhs {
                if lhs.source != rhs.source {
                    panic!("uhhhhhhh, {left:?}, {right:?}")
                } else {
                    return self.node(NodeInfoRef {
                        span: lhs.span.combine(rhs.span),
                        source: lhs.source,
                        parent: lhs.parent,
                    });
                }
            }
        }
        left
    }

    pub fn alloc_str(&self, data: impl AsRef<str>) -> &'a str {
        self.bump.alloc_str(data.as_ref())
    }

    pub fn alloc<T: Copy>(&self, data: T) -> &'a T {
        self.bump.alloc(data)
    }

    pub fn alloc_slice<T: Copy>(&self, data: &[T]) -> &'a [T] {
        self.bump.alloc_slice_copy(data)
    }

    pub fn eof(&self) -> NodeRef<'a> {
        self.top_src_eof
    }

    pub fn src(&self) -> SourceRef<'a> {
        self.top_src
    }

    pub fn node(&self, node: NodeInfoRef<'a>) -> NodeRef<'a> {
        self.bump.alloc(node)
    }

    pub fn node_to_owned(&mut self, node: NodeRef<'a>) -> NodeOwned {
        let node_ptr = node as *const NodeInfoRef<'a>;
        if let Some(node) = self.owned_node_map.get(&node_ptr) {
            return node.clone();
        }

        let src_ptr = node.source as *const SourceInfoRef<'a>;
        let source = if let Some(src) = self.owned_source_map.get(&src_ptr) {
            src.clone()
        } else {
            let owned = SourceOwned {
                path: node.source.path.into(),
                contents: node.source.contents.into(),
            };

            self.owned_source_map.insert(src_ptr, owned.clone());

            owned
        };

        let parent = match node.parent {
            Parent::None => Parent::None,
            Parent::Included { parent } => Parent::Included {
                parent: self.node_to_owned(parent),
            },
            Parent::Pasted { parent, definition } => Parent::Pasted {
                parent: self.node_to_owned(parent),
                definition: definition.map(|def| self.node_to_owned(def)),
            },
            Parent::Generated { parent, definition } => Parent::Generated {
                parent: self.node_to_owned(parent),
                definition: definition.map(|def| self.node_to_owned(def)),
            },
        };

        let owned = Arc::new(NodeInfoOwned {
            span: node.span,
            source,
            parent,
        });

        self.owned_node_map.insert(node_ptr, owned.clone());

        owned
    }

    pub fn report_owned(&mut self, entry: LogEntry<NodeOwned>) {
        self.logs.report(entry);
    }

    pub fn report(&mut self, entry: LogEntry<NodeRef<'a>>) {
        let parts = entry
            .parts
            .into_iter()
            .map(|part| LogPart {
                node: part.node.map(|n| self.node_to_owned(n)),
                kind: part.kind,
                msg: part.msg,
            })
            .collect();
        self.logs.report(LogEntry { parts });
    }

    pub fn report_error_locless(&mut self, msg: impl ToString) {
        self.report(LogEntry::new().error_locless(msg));
    }

    pub fn report_warning_locless(&mut self, msg: impl ToString) {
        self.report(LogEntry::new().warning_locless(msg));
    }

    pub fn report_info_locless(&mut self, msg: impl ToString) {
        self.report(LogEntry::new().info_locless(msg));
    }

    pub fn report_hint_locless(&mut self, msg: impl ToString) {
        self.report(LogEntry::new().hint_locless(msg));
    }

    pub fn report_error(&mut self, node: NodeRef<'a>, error: impl ToString) {
        self.report(LogEntry::new().error(node, error));
    }

    pub fn report_warning(&mut self, node: NodeRef<'a>, error: impl ToString) {
        self.report(LogEntry::new().warning(node, error));
    }

    pub fn report_info(&mut self, node: NodeRef<'a>, error: impl ToString) {
        self.report(LogEntry::new().info(node, error));
    }

    pub fn report_error_eof(&mut self, error: impl ToString) {
        self.report(LogEntry::new().error(self.top_src_eof, error));
    }

    pub fn report_warning_eof(&mut self, error: impl ToString) {
        self.report(LogEntry::new().warning(self.top_src_eof, error));
    }

    pub fn report_info_eof(&mut self, error: impl ToString) {
        self.report(LogEntry::new().info(self.top_src_eof, error));
    }

    pub fn unexpected_token(
        &mut self,
        got: Option<Node<'a, Token<'a>>>,
        expected: impl Display,
        alternate: bool,
    ) -> NodeRef<'a> {
        if alternate {
            match got {
                Some(Node(got, n)) => self.report_error(
                    n,
                    format!("unexpected token, got {got:#} expected {expected:#}"),
                ),
                None => self
                    .report_error_eof(format!("unexpected token, got eof expected {expected:#}")),
            }
        } else {
            match got {
                Some(Node(got, n)) => self.report_error(
                    n,
                    format!("unexpected token, got {got:#} expected {expected}"),
                ),
                None => {
                    self.report_error_eof(format!("unexpected token, got eof expected {expected}"))
                }
            }
        }

        match got {
            Some(Node(_, n)) => n,
            None => self.eof(),
        }
    }

    pub fn take_logs(&mut self) -> Logs<NodeOwned> {
        self.logs.take()
    }
}
