pub mod lang;

use crate::assembler::lang::AssemblyLanguage;
use crate::context::{Context, NodeRef};
use crate::expression::ExpressionEvaluator;
use crate::{context::Node, lex::Token, preprocess::PreProcessor};

pub struct LangCtx<'a, 'b, T: AssemblyLanguage<'a>> {
    pub context: &'b mut Context<'a>,
    pub preprocessor: &'b mut PreProcessor<'a, T>,
}
impl<'a, 'b, T: AssemblyLanguage<'a>> LangCtx<'a, 'b, T> {
    pub fn new(context: &'b mut Context<'a>, preprocessor: &'b mut PreProcessor<'a, T>) -> Self {
        Self {
            context,
            preprocessor,
        }
    }

    pub fn asm<'c>(&'c mut self, lang: &'c mut T) -> Assembler<'a, 'c, T> {
        Assembler {
            context: self.context,
            lang,
            preprocessor: self.preprocessor,
        }
    }

    pub fn eval<'c>(&'c mut self, lang: &'c mut T) -> ExpressionEvaluator<'a, 'c, T> {
        ExpressionEvaluator::new(
            self.context,
            lang,
            self.preprocessor,
            crate::expression::ExprKind::Lang,
        )
    }
}
pub struct PreProcessorCtx<'a, 'b, T: AssemblyLanguage<'a>> {
    pub context: &'b mut Context<'a>,
    pub lang: &'b mut T,
}
impl<'a, 'b, T: AssemblyLanguage<'a>> PreProcessorCtx<'a, 'b, T> {
    pub fn new(context: &'b mut Context<'a>, lang: &'b mut T) -> Self {
        Self { context, lang }
    }

    pub fn asm<'c>(
        &'c mut self,
        preprocessor: &'c mut PreProcessor<'a, T>,
    ) -> Assembler<'a, 'c, T> {
        Assembler {
            context: self.context,
            lang: self.lang,
            preprocessor,
        }
    }

    pub fn eval<'c>(
        &'c mut self,
        preprocessor: &'c mut PreProcessor<'a, T>,
    ) -> ExpressionEvaluator<'a, 'c, T> {
        ExpressionEvaluator::new(
            self.context,
            self.lang,
            preprocessor,
            crate::expression::ExprKind::PreProcessor,
        )
    }
}

pub struct Assembler<'a, 'b, T: AssemblyLanguage<'a>> {
    pub context: &'b mut Context<'a>,
    pub lang: &'b mut T,
    pub preprocessor: &'b mut PreProcessor<'a, T>,
}

impl<'a, 'b, T: AssemblyLanguage<'a>> Assembler<'a, 'b, T> {
    pub fn assemble(
        context: &'b mut Context<'a>,
        lang: &'b mut T,
        preprocessor: &'b mut PreProcessor<'a, T>,
    ) -> T::AssembledResult {
        Self {
            context,
            lang,
            preprocessor,
        }
        ._assemble()
    }

    fn _assemble(&mut self) -> T::AssembledResult {
        self.preprocessor
            .begin(PreProcessorCtx::new(self.context, self.lang));

        while let Some(Node(Token::NewLine | Token::Semicolon, _)) = self.peek() {
            self.next();
        }
        while self.peek().is_some() {
            self.assemble_line();
            while let Some(Node(Token::NewLine | Token::Semicolon, _)) = self.peek() {
                self.next();
            }
        }

        self.lang
            .finish(LangCtx::new(self.context, self.preprocessor))
    }

    pub fn split_preprocessor(self) -> (&'b mut PreProcessor<'a, T>, PreProcessorCtx<'a, 'b, T>) {
        (
            self.preprocessor,
            PreProcessorCtx::new(self.context, self.lang),
        )
    }

    pub fn split_lang(self) -> (&'b mut T, LangCtx<'a, 'b, T>) {
        (self.lang, LangCtx::new(self.context, self.preprocessor))
    }

    fn assemble_line(&mut self) {
        match self.next() {
            Some(Node(Token::Ident(ident), n)) => {
                self.lang.assemble_mnemonic(
                    &mut LangCtx::new(self.context, self.preprocessor),
                    ident,
                    n,
                );
                loop {
                    match self.peek() {
                        None | Some(Node(Token::NewLine | Token::Semicolon, _)) => break,
                        Some(Node(t, n)) => self.context.report_error(
                            n,
                            format!("unexpected token '{t:#}' at end of statement"),
                        ),
                    }
                    self.next();
                }
            }
            Some(Node(Token::Label(label), source)) => {
                self.lang.encounter_label(
                    &mut LangCtx::new(self.context, self.preprocessor),
                    label,
                    source,
                );
            }
            Some(Node(t, n)) => self.context.report_error(
                n,
                format!("unexpected token {t:#} expected identifier or label"),
            ),
            None => {}
        }
    }

    pub fn unknown_mnemonic(&mut self, mnemonic: &'a str, n: NodeRef<'a>) {
        self.context
            .report_error(n, format!("unrecognized mnemonic '{mnemonic}'"));

        while !matches!(
            self.peek(),
            None | Some(Node(Token::NewLine | Token::Semicolon, _))
        ) {
            self.next();
        }
    }

    fn next(&mut self) -> Option<Node<'a, Token<'a>>> {
        self.preprocessor
            .next(&mut PreProcessorCtx::new(self.context, self.lang))
    }

    fn peek(&mut self) -> Option<Node<'a, Token<'a>>> {
        self.preprocessor
            .peek(&mut PreProcessorCtx::new(self.context, self.lang))
    }
}
