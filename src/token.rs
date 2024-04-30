use logos::Lexer;
use logos::Logos;
pub use TokenKind::*;

use crate::parser::Span;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Token<'a> {
    pub source: &'a str,
    pub kind: TokenKind,
    pub span: Span,
}

impl<'a> Token<'a> {
    pub fn new_eoi(source: &'a str) -> Self {
        Token {
            source,
            kind: TokenKind::EOI,
            span: (source.len()..source.len()).into(),
        }
    }

    pub fn text(&self) -> &'a str {
        &self.source[std::ops::Range::from(self.span)]
    }
}

impl<'a> std::fmt::Debug for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}({:?})", self.kind, self.span)
    }
}

pub struct Tokenizer<'a> {
    source: &'a str,
    lexer: Lexer<'a, TokenKind>,
    prev_token: Option<TokenKind>,
    eoi: bool,
}

impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a str) -> Self {
        Tokenizer {
            source,
            lexer: TokenKind::lexer(source),
            eoi: false,
            prev_token: None,
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lexer.next() {
            Some(Ok(kind)) => {
                let span = (self.lexer.span().start..self.lexer.span().end).into();
                let token = Token {
                    source: self.source,
                    kind,
                    span,
                };
                self.prev_token = Some(kind);
                Some(token)
            }
            Some(Err(())) if self.prev_token == Some(TokenKind::Error) => {
                self.eoi = true;
                None
            }
            Some(Err(())) => {
                let span = (self.lexer.span().start..self.lexer.span().end).into();
                let token = Token {
                    source: self.source,
                    kind: TokenKind::Error,
                    span,
                };
                self.prev_token = Some(TokenKind::Error);
                Some(token)
            }
            None if !self.eoi => {
                self.eoi = true;
                Some(Token::new_eoi(self.source))
            }
            None => None,
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Logos, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TokenKind {
    EOI,

    Error,

    #[regex(r"[ \t\r\n\f]+", logos::skip)]
    Whitespace,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token(",")]
    Comma,
    #[token("+")]
    Plus,
    #[token(";")]
    Semicolon,

    #[regex(r#"[_a-zA-Z][_$a-zA-Z0-9]*"#)]
    Ident,

    #[regex(r#"`[^`]*`"#)]
    #[regex(r#""([^"\\]|\\.|"")*""#)]
    #[regex(r#"'([^'\\]|\\.|'')*'"#)]
    QuotedString,

    #[regex(r"[0-9]+(_|[0-9])*")]
    LiteralInteger,

    #[token("SELECT", ignore(ascii_case))]
    SELECT,
    #[token("FROM", ignore(ascii_case))]
    FROM,
}
