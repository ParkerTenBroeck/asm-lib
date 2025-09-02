use std::collections::HashMap;
use std::fmt::Write;
use std::path::Path;

use crate::assembler::PreProcessorCtx;
use crate::assembler::lang::AssemblyLanguage;
use crate::context::NodeInfoRef;
use crate::context::Parent;
use crate::expression::Value;
use crate::expression::ValueType;
use crate::expression::args::PathArg;
use crate::lex::Spanned;
use crate::logs::LogEntry;
use crate::{
    context::{Node, NodeRef, SourceRef},
    lex::Token,
};

#[derive(Clone, Copy)]
pub struct SrcSlice<'a> {
    pub contents: &'a [Spanned<Token<'a>>],
    pub src: SourceRef<'a>,
}

impl<'a> Iterator for SrcSlice<'a> {
    type Item = &'a Spanned<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        let (first, rem) = self.contents.split_first()?;
        self.contents = rem;
        Some(first)
    }
}

impl<'a> SrcSlice<'a> {
    pub fn peek(&mut self) -> Option<&'a Spanned<Token<'a>>> {
        Some(self.contents.split_first()?.0)
    }

    pub fn make<T: AssemblyLanguage<'a>>(
        &mut self,
        ctx: &mut PreProcessorCtx<'a, '_, T>,
        parent: Parent<NodeRef<'a>>,
    ) -> Option<Node<'a, Token<'a>>> {
        let token = self.next()?;
        Some(Node(
            token.val,
            ctx.context.node(NodeInfoRef {
                span: token.span,
                source: self.src,
                parent,
            }),
        ))
    }

    pub fn peek_make<T: AssemblyLanguage<'a>>(
        &mut self,
        ctx: &mut PreProcessorCtx<'a, '_, T>,
        parent: Parent<NodeRef<'a>>,
    ) -> Option<Node<'a, Token<'a>>> {
        let token = self.peek()?;
        Some(Node(
            token.val,
            ctx.context.node(NodeInfoRef {
                span: token.span,
                source: self.src,
                parent,
            }),
        ))
    }

    pub fn new(contents: &'a [Spanned<Token<'a>>], src: SourceRef<'a>) -> Self {
        Self { contents, src }
    }
}

pub struct Stage<'a, T: AssemblyLanguage<'a>> {
    pub contents: SrcSlice<'a>,
    pub parent: Parent<NodeRef<'a>>,
    pub filter: Option<Box<dyn PreProcessorFilter<'a, T> + 'a>>,
}

impl<'a, T: AssemblyLanguage<'a>> Iterator for Stage<'a, T> {
    type Item = &'a Spanned<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.contents.next()
    }
}

impl<'a, T: AssemblyLanguage<'a>> Stage<'a, T> {
    pub fn peek(&mut self) -> Option<&'a Spanned<Token<'a>>> {
        self.contents.peek()
    }

    pub fn next_make(
        &mut self,
        ctx: &mut PreProcessorCtx<'a, '_, T>,
    ) -> Option<Node<'a, Token<'a>>> {
        self.contents.make(ctx, self.parent)
    }

    pub fn peek_make(
        &mut self,
        ctx: &mut PreProcessorCtx<'a, '_, T>,
    ) -> Option<Node<'a, Token<'a>>> {
        self.contents.peek_make(ctx, self.parent)
    }
}
pub enum FilterResult<'a> {
    Pass(Option<Node<'a, Token<'a>>>),
    Fail,
}

pub trait PreProcessorFilter<'a, T: AssemblyLanguage<'a>> {
    fn filter(
        &mut self,
        ctx: &mut PreProcessorCtx<'a, '_, T>,
        token: Option<Node<'a, Token<'a>>>,
    ) -> FilterResult<'a>;
}

pub struct MacroDef<'a> {
    definition: NodeRef<'a>,
    contents: SrcSlice<'a>,
}

pub struct PreProcessor<'a, T: AssemblyLanguage<'a>> {
    stack: Vec<Stage<'a, T>>,
    macros: HashMap<&'a str, MacroDef<'a>>,

    peek: Option<Node<'a, Token<'a>>>,
}

impl<'a, T: AssemblyLanguage<'a>> Default for PreProcessor<'a, T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, T: AssemblyLanguage<'a>> PreProcessor<'a, T> {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            macros: HashMap::new(),

            peek: None,
        }
    }

    fn add_stage(&mut self, ctx: &mut PreProcessorCtx<'a, '_, T>, stage: Stage<'a, T>) {
        let limit = ctx.context.config().preprocessor_stack_limit;
        if self.stack.len() > limit {
            if let Some(source) = stage.parent.parent() {
                ctx.context.report_error(
                    source,
                    format!("preprocessor stack recursion limit hit ({limit})",),
                );
            } else {
                ctx.context.report_error_locless(format!(
                    "preprocessor stack recursion limit hit ({limit})"
                ));
            }
            self.stack.clear();
        } else {
            self.stack.push(stage);
        }
    }

    pub fn begin(&mut self, mut ctx: PreProcessorCtx<'a, '_, T>) {
        let path = ctx.context.src().path;
        self.include(&mut ctx, path, Parent::None);
    }

    pub fn top_parent(&mut self) -> Parent<NodeRef<'a>> {
        self.stack.last().map(|p| p.parent).unwrap_or(Parent::None)
    }

    pub fn parse_block(&mut self, ctx: &mut PreProcessorCtx<'a, '_, T>) -> SrcSlice<'a> {
        let Some(top) = self.stack.last_mut() else {
            return SrcSlice::new(&[], ctx.context.src());
        };

        let first = if let Some(peek) = self.peek
            && peek.1.parent == top.parent
        {
            self.peek.take()
        } else {
            top.next_make(ctx)
        };
        match first {
            Some(Node(Token::LBrace, _)) => {}
            got => {
                ctx.context.unexpected_token(got, Token::LBrace, false);
            }
        }

        let slice = top.contents.contents;
        let mut size = 0;
        let mut indent = 0usize;
        loop {
            match top.next() {
                Some(Spanned {
                    val: Token::LBrace, ..
                }) => indent += 1,
                Some(Spanned {
                    val: Token::RBrace, ..
                }) if indent == 0 => break,
                Some(Spanned {
                    val: Token::RBrace, ..
                }) => indent -= 1,
                None => {
                    let node = ctx.context.node(NodeInfoRef {
                        span: top.contents.src.eof(),
                        source: top.contents.src,
                        parent: top.parent,
                    });
                    ctx.context.report_error(node, "unclosed block");
                    break;
                }
                _ => {}
            }
            size += 1;
        }

        SrcSlice::new(&slice[..size], top.contents.src)
    }

    pub fn include(
        &mut self,
        ctx: &mut PreProcessorCtx<'a, '_, T>,
        path: &'a Path,
        parent: Parent<NodeRef<'a>>,
    ) -> Option<SourceRef<'a>> {
        let src = crate::lex::lex_file(ctx.context, path, parent)?;
        self.add_stage(
            ctx,
            Stage {
                contents: src,
                parent,
                filter: None,
            },
        );
        Some(src.src)
    }

    fn stack_next(&mut self, ctx: &mut PreProcessorCtx<'a, '_, T>) -> Option<Node<'a, Token<'a>>> {
        if self.peek.is_some() {
            return self.peek.take();
        }
        while let Some(top) = self.stack.last_mut() {
            if let Some(next) = top.next_make(ctx) {
                match next {
                    Node(Token::MultiLineComment(str), n)
                    | Node(Token::SingleLineComment(str), n) => {
                        let (lang, mut ctx) = ctx.asm(self).split_lang();
                        lang.encounter_comment(&mut ctx, str, n);
                        continue;
                    }
                    _ => {}
                }
                return Some(next);
            } else {
                self.stack.pop();
            }
        }
        None
    }

    fn parse_if(&mut self, ctx: &mut PreProcessorCtx<'a, '_, T>, _: NodeRef<'a>) {
        let res = ctx.eval(self).expr(ValueType::Bool);

        let parent = self.top_parent();
        let mut result = None;
        {
            let contents = self.parse_block(ctx);
            if res.0.is_true() {
                result = Some(contents);
            }
        }

        let mut else_encountered = None;
        loop {
            match self.stack_next(ctx) {
                Some(Node(Token::PreProcessorTag("else"), n)) => {
                    let contents = self.parse_block(ctx);
                    if let Some(other) = else_encountered {
                        ctx.context.report(
                            LogEntry::new()
                                .error(n, "else can only appear as the last block in if chain")
                                .info(other, "first encounter here"),
                        );
                    } else {
                        if result.is_none() {
                            result = Some(contents);
                        }
                        else_encountered = Some(n);
                    }
                }
                Some(Node(Token::PreProcessorTag("elseif"), n)) => {
                    let res = ctx.eval(self).expr(ValueType::Bool);
                    let contents = self.parse_block(ctx);
                    if let Some(other) = else_encountered {
                        ctx.context.report(
                            LogEntry::new()
                                .error(n, "else can only appear as the last block in if chain")
                                .info(other, "first encounter here"),
                        );
                    } else if res.0.is_true() && result.is_none() {
                        result = Some(contents);
                    }
                }
                v => {
                    self.peek = v;
                    break;
                }
            }
        }

        if let Some(contents) = result {
            self.stack.push(Stage {
                contents,
                parent,
                filter: None,
            });
        }
    }

    #[allow(unused)]
    fn parse_macro_args(&mut self, ctx: &mut PreProcessorCtx<'a, '_, T>) {}

    #[allow(unused)]
    fn parse_macro_parameters(&mut self, ctx: &mut PreProcessorCtx<'a, '_, T>) {}

    fn handle_preprocessor_tag(
        &mut self,
        ctx: &mut PreProcessorCtx<'a, '_, T>,
        tag: &'a str,
        n: NodeRef<'a>,
    ) -> Option<Node<'a, Token<'a>>> {
        match tag {
            "include" => {
                if let Node(PathArg::Val(Some(path)), n) = ctx.eval(self).coerced(n) {
                    self.include(ctx, path, Parent::Included { parent: n });
                }
            }
            "def" => match self.stack_next(ctx) {
                Some(Node(Token::Ident(str), node)) => {
                    return Some(Node(
                        if self.macros.contains_key(str) {
                            Token::TrueLiteral
                        } else {
                            Token::FalseLiteral
                        },
                        ctx.context.merge_nodes(n, node),
                    ));
                }
                t => _ = ctx.context.unexpected_token(t, Token::Ident(""), false),
            },
            "if" => self.parse_if(ctx, n),
            "concat" => {
                let Node(parts, node): Node<'_, Vec<Value<'a, T>>> =
                    ctx.eval(self).coerced_delim(n, Token::LPar, Token::RPar);
                let mut ident = String::new();
                for part in parts.into_iter() {
                    ident.write_fmt(format_args!("{part}")).unwrap();
                }
                let ident = ctx.context.alloc_str(ident);
                return Some(Node(Token::Ident(ident), node));
            }
            "macro" => {
                let (ident, definition) = match self.stack_next(ctx) {
                    Some(Node(Token::Ident(str), node)) => (str, node),
                    t => ("", ctx.context.unexpected_token(t, Token::Ident(""), false)),
                };
                // let args = self.parse_macro_parameters(ctx);

                let block = self.parse_block(ctx);
                if !ident.is_empty() {
                    self.macros.insert(
                        ident,
                        MacroDef {
                            definition,
                            contents: block,
                        },
                    );
                }
            }

            unknown => {
                ctx.context
                    .report_error(n, format!("unknown preprocessor tag '{unknown}'"));
            }
        }
        None
    }

    fn handle_identifier(
        &mut self,
        ctx: &mut PreProcessorCtx<'a, '_, T>,
        ident: &'a str,
        n: NodeRef<'a>,
    ) -> bool {
        if let Some(value) = self.macros.get(ident) {
            self.add_stage(
                ctx,
                Stage {
                    contents: value.contents,
                    parent: Parent::Pasted {
                        parent: n,
                        definition: Some(value.definition),
                    },
                    filter: None,
                },
            );
            return false;
        }
        true
    }

    fn next_filtered(
        &mut self,
        ctx: &mut PreProcessorCtx<'a, '_, T>,
    ) -> Option<Node<'a, Token<'a>>> {
        if self.peek.is_some() {
            return self.peek.take();
        }
        'outer: loop {
            let mut next = self.stack_next(ctx);
            for stage in self.stack.iter_mut().rev() {
                if let Some(filter) = &mut stage.filter {
                    match filter.filter(ctx, next) {
                        FilterResult::Pass(node) => next = node,
                        FilterResult::Fail => continue 'outer,
                    }
                }
            }

            match next {
                Some(Node(Token::PreProcessorTag(tag), n)) => {
                    if let Some(tok) = self.handle_preprocessor_tag(ctx, tag, n) {
                        return Some(tok);
                    }
                }
                t @ Some(Node(Token::Ident(ident), n)) => {
                    if self.handle_identifier(ctx, ident, n) {
                        return t;
                    }
                }
                other => return other,
            }
        }
    }

    pub fn peek(&mut self, ctx: &mut PreProcessorCtx<'a, '_, T>) -> Option<Node<'a, Token<'a>>> {
        if self.peek.is_none() {
            self.peek = self.next(ctx);
        }
        self.peek
    }

    pub fn next(&mut self, ctx: &mut PreProcessorCtx<'a, '_, T>) -> Option<Node<'a, Token<'a>>> {
        if self.peek.is_none() {
            self.peek = self.next_filtered(ctx);
        }
        self.peek.take()
    }
}
