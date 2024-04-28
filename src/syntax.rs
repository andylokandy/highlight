use std::ops::Range;

use crossterm::style::Color;

use crate::{
    parser::{alt, map, match_token, opt, sequence, FromParseError, Input, Solutions},
    token::*,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn merge(&self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Span {
            start: range.start,
            end: range.end,
        }
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.start..span.end
    }
}

#[derive(Debug, Clone)]
pub enum Semantic {
    Keyword,
    Ident,
    Table,
    Literal,
    Operator,
    Bracket,
    Error(String),
}

impl Semantic {
    pub fn backgrourd_color(&self) -> Color {
        match self {
            Semantic::Keyword => Color::Blue,
            Semantic::Ident => Color::Green,
            Semantic::Table => Color::DarkYellow,
            Semantic::Literal => Color::Yellow,
            Semantic::Operator => Color::DarkCyan,
            Semantic::Bracket => Color::Cyan,
            Semantic::Error(_) => Color::DarkRed,
        }
    }

    pub fn foreground_color(&self) -> Color {
        match self {
            Semantic::Keyword => Color::Black,
            Semantic::Ident => Color::Black,
            Semantic::Table => Color::Black,
            Semantic::Literal => Color::Black,
            Semantic::Operator => Color::Black,
            Semantic::Bracket => Color::Black,
            Semantic::Error(_) => Color::White,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Error {
    ExpectToken(TokenKind),
    Missing(String),
}

impl FromParseError for Error {
    fn match_error(kind: TokenKind) -> Self {
        Error::ExpectToken(kind)
    }

    fn missing_error(label: String) -> Self {
        Error::Missing(label)
    }
}

pub fn query(text: &str) -> Vec<(Span, Semantic)> {
    let tokens = Tokenizer::new(text).collect::<Vec<_>>();

    let input = Input {
        tokens: &tokens,
        fast_path: true,
    };

    let fast_res = select_stmt(input);

    dbg!(&fast_res);

    let input = Input {
        tokens: &tokens,
        fast_path: false,
    };
    let res = select_stmt(input);

    dbg!(&res);

    assert_eq!(
        fast_res
            .best()
            .as_ref()
            .and_then(|solution| solution.value.clone()),
        res.best()
            .as_ref()
            .and_then(|solution| solution.value.clone())
    );

    if let Some(best) = res.best() {
        best.consumed
            .iter()
            .map(|(span, semantic)| (*span, semantic.clone()))
            .chain(
                best.errors
                    .iter()
                    .map(|(span, err)| (*span, Semantic::Error(format!("{err:?}")))),
            )
            .collect()
    } else {
        vec![]
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SelectStmt {
    pub expr: Expr,
    pub table: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(String),
    FuncCall(String, i64),
}

pub fn select_stmt(i: Input) -> Solutions<SelectStmt, Semantic, Error> {
    let select = match_token(SELECT, Semantic::Keyword);
    let ident = match_token(Ident, Semantic::Ident);
    let from = match_token(FROM, Semantic::Keyword);
    let table_ident = match_token(Ident, Semantic::Table);
    let semi = match_token(Semicolon, Semantic::Bracket);
    let eoi = match_token(EOI, Semantic::Bracket);

    let lparen = match_token(LParen, Semantic::Bracket);
    let rparen = match_token(RParen, Semantic::Bracket);
    let lit_num = match_token(LiteralInteger, Semantic::Literal);
    let func_call = map(
        sequence(
            match_token(Ident, Semantic::Ident),
            sequence(lparen, sequence(lit_num, rparen)),
        ),
        |(name, (_, (lit, _)))| {
            Expr::FuncCall(name.text().to_string(), lit.text().parse::<i64>().unwrap())
        },
    );

    let expr_ident = map(ident, |ident| Expr::Ident(ident.text().to_string()));

    map(
        sequence(
            select,
            sequence(
                alt(func_call, expr_ident),
                // func_call,
                sequence(opt(sequence(from, table_ident)), sequence(semi, eoi)),
                // sequence(semi, eoi),
            ),
        ),
        // |_| SelectStmt {
        //     expr: Expr::Ident("".to_string()),
        //     table: None,
        // },
        |(_, (expr, (opt_from, _)))| SelectStmt {
            expr,
            table: opt_from.map(|(_, table)| table.text().to_string()),
        },
    )(i)
}
