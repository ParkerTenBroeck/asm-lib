pub mod args;
pub mod base;
pub mod conversion;
pub mod str;
pub mod value;

pub use base::*;
pub use str::*;
pub use value::*;

use crate::assembler::PreProcessorCtx;
use crate::assembler::lang::AssemblyLanguage;
use crate::context::{Context, Node, NodeRef};
use crate::expression::args::CoercedArgs;
use crate::lex::{Number, Token};
use crate::logs::LogEntry;
use crate::preprocess::PreProcessor;
use crate::util::IntoStrDelimable;

pub type NodeVal<'a, T> = Node<'a, Value<'a, T>>;

#[derive(Clone, Copy)]
pub enum ExprKind {
    Assembler,
    PreProcessor,
    Lang,
}

pub struct FuncParamParser<'a, 'b> {
    func: Node<'a, &'a str>,
    func_node: &'b mut Option<NodeRef<'a>>,
}

impl<'a, 'b> FuncParamParser<'a, 'b> {
    pub fn func(&self) -> &'a str {
        self.func.0
    }

    pub fn coerced_args<A: CoercedArgs<'a, L>, L: AssemblyLanguage<'a>>(
        self,
        lang: &mut L,
        ctx: &mut ExprCtx<'a, '_, L>,
    ) -> Node<'a, A> {
        let args =
            A::coerced_args_delim(&mut ctx.eval(lang), self.func.1, Token::LPar, Token::RPar);
        *self.func_node = Some(args.1);
        args
    }

    pub fn args<L: AssemblyLanguage<'a>>(
        self,
        lang: &mut L,
        ctx: &mut ExprCtx<'a, '_, L>,
    ) -> Node<'a, Vec<Node<'a, Value<'a, L>>>> {
        let args = ctx.eval(lang).args_delim(
            self.func.1,
            Token::LPar,
            Token::RPar,
            ArgumentsTypeHint::None,
        );
        *self.func_node = Some(args.1);
        args
    }
}

pub struct ExprCtx<'a, 'b, T: AssemblyLanguage<'a>> {
    pub context: &'b mut Context<'a>,
    pub preprocessor: &'b mut PreProcessor<'a, T>,
    kind: ExprKind,
}

impl<'a, 'b, L: AssemblyLanguage<'a>> ExprCtx<'a, 'b, L> {
    pub fn eval<'c>(&'c mut self, lang: &'c mut L) -> ExpressionEvaluator<'a, 'c, L> {
        ExpressionEvaluator {
            context: self.context,
            lang,
            preprocessor: self.preprocessor,
            kind: self.kind,
        }
    }
}

macro_rules! ctx {
    ($self:expr) => {
        &mut ExprCtx {
            context: $self.context,
            preprocessor: $self.preprocessor,
            kind: $self.kind,
        }
    };
}

pub struct ExpressionEvaluator<'a, 'b, T: AssemblyLanguage<'a>> {
    pub context: &'b mut Context<'a>,
    pub lang: &'b mut T,
    pub preprocessor: &'b mut PreProcessor<'a, T>,
    kind: ExprKind,
}

impl<'a, 'b, L: AssemblyLanguage<'a>> ExpressionEvaluator<'a, 'b, L> {
    pub fn new(
        context: &'b mut Context<'a>,
        lang: &'b mut L,
        preprocessor: &'b mut PreProcessor<'a, L>,
        kind: ExprKind,
    ) -> Self {
        Self {
            context,
            lang,
            preprocessor,
            kind,
        }
    }

    pub fn split_ctx(&mut self) -> (&mut L, ExprCtx<'a, '_, L>) {
        (
            self.lang,
            ExprCtx {
                context: self.context,
                preprocessor: self.preprocessor,
                kind: self.kind,
            },
        )
    }

    pub fn expr(&mut self, hint: ValueType<'a, L>) -> Node<'a, Value<'a, L>> {
        self.parse_expr(hint)
    }

    pub fn args(
        &mut self,
        init: NodeRef<'a>,
        hint: ArgumentsTypeHint<'a, '_, L>,
    ) -> Node<'a, Vec<NodeVal<'a, L>>> {
        self.parse_arguments(init, hint)
    }

    pub fn args_delim(
        &mut self,
        init: NodeRef<'a>,
        start: Token<'a>,
        end: Token<'a>,
        hint: ArgumentsTypeHint<'a, '_, L>,
    ) -> Node<'a, Vec<NodeVal<'a, L>>> {
        self.parse_arguments_delim(init, start, end, hint)
    }

    pub fn coerced<T: CoercedArgs<'a, L>>(&mut self, init: NodeRef<'a>) -> Node<'a, T>
    where
        Self: Sized,
    {
        T::coerced_args(self, init)
    }

    pub fn coerced_delim<T: CoercedArgs<'a, L>>(
        &mut self,
        init: NodeRef<'a>,
        start: Token<'a>,
        end: Token<'a>,
    ) -> Node<'a, T>
    where
        Self: Sized,
    {
        T::coerced_args_delim(self, init, start, end)
    }

    fn next(&mut self) -> Option<Node<'a, Token<'a>>> {
        self.preprocessor
            .next(&mut PreProcessorCtx::new(self.context, self.lang))
    }
    fn peek(&mut self) -> Option<Node<'a, Token<'a>>> {
        self.preprocessor
            .peek(&mut PreProcessorCtx::new(self.context, self.lang))
    }

    fn parse_expr_1(&mut self, hint: ValueType<'a, L>, negated: bool) -> NodeVal<'a, L> {
        let expr = match self.next() {
            Some(Node(Token::Ident(ident), node)) => match self.peek() {
                Some(Node(Token::LPar, _)) => {
                    let mut loc = None;
                    let res = self.lang.eval_func(
                        ctx!(self),
                        FuncParamParser {
                            func: Node(ident, node),
                            func_node: &mut loc,
                        },
                        hint,
                    );

                    let loc = if let Some(loc) = loc {
                        loc
                    } else {
                        let loc = self.parse_arguments_delim(
                            node,
                            Token::LPar,
                            Token::RPar,
                            ArgumentsTypeHint::None,
                        );
                        self.context.report(LogEntry::new()
                            .error(loc.1, "function arguments never consumer"
                        ).info_locless("this is an internal assembler error, if possible please make an issue"));
                        loc.1
                    };

                    Node(res, loc)
                }
                _ => Node(
                    self.lang.parse_ident(ctx!(self), Node(ident, node), hint),
                    node,
                ),
            },
            Some(Node(Token::TrueLiteral, node)) => {
                Node(Value::Constant(Constant::Bool(true)), node)
            }
            Some(Node(Token::FalseLiteral, node)) => {
                Node(Value::Constant(Constant::Bool(false)), node)
            }
            Some(Node(Token::StringLiteral(str), node)) => {
                use crate::lex::str::StringKind;
                use crate::lex::str::TokenString;
                Node(
                    Value::Constant(match str {
                        TokenString::Unparsed(str, char_kind) => {
                            self.context.report_error(node, "encountered unparsed string literal during expression evaluation. this is a error in the assembler please report an issue");
                            match char_kind {
                                StringKind::Regular => Constant::Str(AsmStr::Str(str)),
                                StringKind::Byte => Constant::Str(AsmStr::ByteStr(str.as_bytes())),
                                StringKind::CStr => Constant::Str(AsmStr::CStr(str.as_bytes())),
                            }
                        }
                        TokenString::ParsedReg(s) => Constant::Str(AsmStr::Str(s)),
                        TokenString::ParsedByte(b) => Constant::Str(AsmStr::ByteStr(b)),
                        TokenString::ParsedC(c) => Constant::Str(AsmStr::CStr(c)),
                    }),
                    node,
                )
            }
            Some(Node(Token::CharLiteral(char), node)) => {
                use crate::lex::str::CharKind;
                use crate::lex::str::TokenChar;
                Node(
                    Value::Constant(match char {
                        TokenChar::Unparsed(_, char_kind) => {
                            self.context.report_error(node, "encountered unparsed char literal during expression evaluation. this is a error in the assembler please report an issue");
                            match char_kind {
                                CharKind::Regular => Constant::Char('\0'),
                                CharKind::Byte => Constant::U8(0),
                            }
                        }
                        TokenChar::ParsedReg(c) => Constant::Char(c),
                        TokenChar::ParsedByte(b) => Constant::U8(b),
                    }),
                    node,
                )
            }
            Some(Node(Token::NumericLiteral(num), node)) => Node(
                self.lang
                    .parse_numeric_literal(ctx!(self), Node(num, node), negated, hint),
                node,
            ),
            Some(Node(Token::LPar, lhs)) => {
                let mut arg = self.parse_expr(hint);
                match self.peek() {
                    Some(Node(Token::RPar, rhs)) => {
                        arg.1 = self.context.merge_nodes(lhs, rhs);
                        self.next();
                    }
                    t => _ = self.context.unexpected_token(t, Token::RPar, false),
                }
                arg
            }

            Some(Node(Token::LBracket, opening)) => {
                let rhs = self.parse_expr(hint);
                let closing = match self.peek() {
                    Some(Node(Token::RBracket, closing)) => {
                        self.next();
                        closing
                    }
                    t => self.context.unexpected_token(t, Token::RBracket, false),
                };
                let node = self.context.merge_nodes(opening, closing);
                return Node(
                    self.lang
                        .eval_index(ctx!(self), node, None, opening, Some(rhs), closing, hint),
                    node,
                );
            }

            t => Node(
                hint.default_value(),
                self.context.unexpected_token(
                    t,
                    [
                        Token::Ident(""),
                        Token::LPar,
                        Token::LBracket,
                        Token::CharLiteral(Default::default()),
                        Token::StringLiteral(Default::default()),
                        Token::FalseLiteral,
                        Token::TrueLiteral,
                        Token::NumericLiteral(Number::EMPTY),
                    ]
                    .iter()
                    .delim("|"),
                    false,
                ),
            ),
        };

        if let Some(Node(Token::LBracket, opening)) = self.peek() {
            self.next();
            let rhs = self.parse_expr(hint);

            let closing = match self.peek() {
                Some(Node(Token::RBracket, closing)) => {
                    self.next();
                    closing
                }
                t => self.context.unexpected_token(t, Token::RBracket, false),
            };
            let node = self.context.merge_nodes(expr.1, closing);
            return Node(
                self.lang.eval_index(
                    ctx!(self),
                    node,
                    Some(expr),
                    opening,
                    Some(rhs),
                    closing,
                    hint,
                ),
                node,
            );
        }

        expr
    }

    fn parse_expr_2(&mut self, hint: ValueType<'a, L>, negated: bool) -> NodeVal<'a, L> {
        match self.peek() {
            Some(Node(Token::Minus, op_node)) => {
                self.next();
                let expr = self.parse_expr_2(hint, !negated);
                let node = self.context.merge_nodes(op_node, expr.1);
                Node(
                    self.lang
                        .eval_unnop(ctx!(self), node, Node(unop::UnOp::Neg, node), expr, hint),
                    node,
                )
            }
            Some(Node(Token::LogicalNot, op_node)) => {
                self.next();
                let expr = self.parse_expr_2(hint, false);
                let node = self.context.merge_nodes(op_node, expr.1);
                Node(
                    self.lang
                        .eval_unnop(ctx!(self), node, Node(unop::UnOp::Not, node), expr, hint),
                    node,
                )
            }
            _ => self.parse_expr_1(hint, negated),
        }
    }

    fn parse_expr_3(&mut self, hint: ValueType<'a, L>) -> NodeVal<'a, L> {
        let expr = self.parse_expr_2(hint, false);
        match self.peek() {
            Some(Node(Token::Ident("as"), as_node)) => {
                self.next();

                let ty = match self.peek() {
                    Some(Node(Token::Ident(ty), node)) => {
                        self.next();
                        Node(ty, node)
                    }

                    t => {
                        self.context.unexpected_token(t, Token::Ident(""), false);
                        return expr;
                    }
                };
                let node = self.context.merge_nodes(expr.1, ty.1);
                Node(
                    self.lang
                        .eval_cast(ctx!(self), node, expr, as_node, ty, hint),
                    node,
                )
            }
            _ => expr,
        }
    }

    fn parse_expr_4(&mut self, hint: ValueType<'a, L>, min_prec: u32) -> NodeVal<'a, L> {
        use Node as N;
        use Token as T;
        use binop::BinOp as BO;
        let mut lhs = self.parse_expr_3(hint);
        loop {
            let op = match self.peek() {
                Some(N(T::Plus, n)) if BO::Add.precedence() >= min_prec => N(BO::Add, n),
                Some(N(T::Minus, n)) if BO::Sub.precedence() >= min_prec => N(BO::Sub, n),
                Some(N(T::Star, n)) if BO::Mul.precedence() >= min_prec => N(BO::Mul, n),
                Some(N(T::Slash, n)) if BO::Div.precedence() >= min_prec => N(BO::Div, n),
                Some(N(T::Percent, n)) if BO::Rem.precedence() >= min_prec => N(BO::Rem, n),
                Some(N(T::LogicalOr, n)) if BO::Or.precedence() >= min_prec => N(BO::Or, n),
                Some(N(T::BitwiseOr, n)) if BO::Or.precedence() >= min_prec => N(BO::Or, n),
                Some(N(T::Ampersand, n)) if BO::And.precedence() >= min_prec => N(BO::And, n),
                Some(N(T::LogicalAnd, n)) if BO::And.precedence() >= min_prec => N(BO::And, n),
                Some(N(T::BitwiseXor, n)) if BO::Xor.precedence() >= min_prec => N(BO::Xor, n),
                Some(N(T::ShiftLeft, n)) if BO::Shl.precedence() >= min_prec => N(BO::Shl, n),
                Some(N(T::ShiftRight, n)) if BO::Shr.precedence() >= min_prec => N(BO::Shr, n),
                Some(N(T::GreaterThan, n)) if BO::Gt.precedence() >= min_prec => N(BO::Gt, n),
                Some(N(T::GreaterThanEq, n)) if BO::Gte.precedence() >= min_prec => N(BO::Gte, n),
                Some(N(T::LessThan, n)) if BO::Lt.precedence() >= min_prec => N(BO::Lt, n),
                Some(N(T::LessThanEq, n)) if BO::Lte.precedence() >= min_prec => N(BO::Lte, n),
                Some(N(T::Equals, n)) if BO::Eq.precedence() >= min_prec => N(BO::Eq, n),
                Some(N(T::NotEquals, n)) if BO::Ne.precedence() >= min_prec => N(BO::Ne, n),
                _ => break,
            };
            self.next();

            let shift = matches!(op.0, BO::Shl | BO::Shr);

            let rhs = self.parse_expr_4(
                if shift { ValueType::U32 } else { hint },
                op.0.precedence() + 1,
            );
            let node = self.context.merge_nodes(lhs.1, rhs.1);
            lhs = Node(
                self.lang.eval_binop(ctx!(self), node, lhs, op, rhs, hint),
                node,
            );
        }
        lhs
    }

    pub fn parse_expr(&mut self, hint: ValueType<'a, L>) -> NodeVal<'a, L> {
        self.parse_expr_4(hint, 0)
    }

    pub fn parse_arguments_delim(
        &mut self,
        init: NodeRef<'a>,
        opening: Token<'a>,
        closing: Token<'a>,
        hint: ArgumentsTypeHint<'a, '_, L>,
    ) -> Node<'a, Vec<NodeVal<'a, L>>> {
        let mut args = Vec::new();

        match self.peek() {
            Some(Node(t, _)) if t == opening => {
                self.next();
            }
            t => {
                self.context.unexpected_token(t, opening, true);
            }
        };
        loop {
            match self.peek() {
                Some(Node(t, end)) if t == closing => {
                    self.next();
                    return Node(args, self.context.merge_nodes(init, end));
                }
                Some(_) => {
                    args.push(self.parse_expr(hint[args.len()]));
                    if let Some(Node(Token::Comma, _)) = self.peek() {
                        self.next();
                    }
                }
                None => {
                    let end = self.context.unexpected_token(
                        None,
                        [Token::Comma, closing].iter().delim("|"),
                        true,
                    );
                    return Node(args, self.context.merge_nodes(init, end));
                }
            }
        }
    }

    pub fn parse_arguments(
        &mut self,
        init: NodeRef<'a>,
        hint: ArgumentsTypeHint<'a, '_, L>,
    ) -> Node<'a, Vec<NodeVal<'a, L>>> {
        let mut args = Vec::new();

        loop {
            match self.peek() {
                Some(Node(Token::NewLine, _)) | None => {
                    if let Some(Node(_, last)) = args.last().copied() {
                        return Node(args, self.context.merge_nodes(init, last));
                    } else {
                        return Node(args, init);
                    }
                }
                _ => {
                    args.push(self.parse_expr(hint[args.len()]));
                    if let Some(Node(Token::Comma, _)) = self.peek() {
                        self.next();
                    }
                }
            }
        }
    }
}
