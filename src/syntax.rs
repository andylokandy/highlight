use crossterm::style::Color as TermColor;

use crate::parser::alt;
use crate::parser::map;
use crate::parser::match_token;
use crate::parser::opt;
use crate::parser::sequence;
use crate::parser::ErrorKind;
use crate::parser::Span;
use crate::token::*;

type Input<'a> = crate::parser::Input<Token<'a>>;
type IResult<'a, O> = crate::parser::IResult<Token<'a>, O, Color, ErrorKind<TokenKind>>;

#[derive(Debug, Clone)]
pub enum Color {
    Keyword,
    Ident,
    Table,
    Literal,
    Bracket,
    Error(String),
}

impl Color {
    pub fn backgrourd_color(&self) -> TermColor {
        match self {
            Color::Keyword => TermColor::Blue,
            Color::Ident => TermColor::Green,
            Color::Table => TermColor::DarkYellow,
            Color::Literal => TermColor::Yellow,
            Color::Bracket => TermColor::Cyan,
            Color::Error(_) => TermColor::DarkRed,
        }
    }

    pub fn foreground_color(&self) -> TermColor {
        match self {
            Color::Keyword => TermColor::Black,
            Color::Ident => TermColor::Black,
            Color::Table => TermColor::Black,
            Color::Literal => TermColor::Black,
            Color::Bracket => TermColor::Black,
            Color::Error(_) => TermColor::White,
        }
    }
}

impl<'a> crate::parser::Token for Token<'a> {
    type TokenKind = TokenKind;
    type TokenStream = &'a [Token<'a>];

    fn span(&self) -> Span {
        self.span
    }

    fn kind(&self) -> Self::TokenKind {
        self.kind
    }
}

pub fn query(text: &str) -> Vec<(Span, Color)> {
    let tokens = Tokenizer::new(text).collect::<Vec<_>>();
    let tokens = tokens.as_slice();

    let input = Input::fast_path(tokens);

    let fast_res = select_stmt(input);

    dbg!(&fast_res);

    let input = Input::recovery_path(tokens);
    let res = select_stmt(input);

    dbg!(&res);

    assert_eq!(fast_res.best().value, res.best().value);

    let best = res.best();
    best.consumed
        .iter()
        .map(|(span, semantic)| (*span, semantic.clone()))
        .chain(
            best.errors
                .iter()
                .map(|(span, err)| (*span, Color::Error(format!("{err:?}")))),
        )
        .collect()
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

pub fn select_stmt(i: Input) -> IResult<SelectStmt> {
    let select = match_keyword(SELECT);
    let ident = |i| match_token(Ident, Color::Ident)(i);
    let from = match_keyword(FROM);
    let semi = match_bracket(Semicolon);
    let eoi = match_bracket(EOI);

    let lparen = match_bracket(LParen);
    let rparen = match_bracket(RParen);
    let lit_num = match_token(LiteralInteger, Color::Literal);
    let func_call = map(
        sequence((ident, lparen, lit_num, rparen)),
        |(name, _, lit, _)| {
            Expr::FuncCall(name.text().to_string(), lit.text().parse::<i64>().unwrap())
        },
    );

    let expr_ident = map(ident, |ident| Expr::Ident(ident.text().to_string()));

    map(
        sequence((
            select,
            opt(sequence((from, table_ident))),
            alt(func_call, expr_ident),
            sequence((
                match_token(Semicolon, Color::Bracket),
                match_token(Semicolon, Color::Bracket),
            )),
            eoi,
        )),
        |_| SelectStmt {
            expr: Expr::Ident("".to_string()),
            table: None,
        },
    )(i)
}

pub fn table_ident(i: Input) -> IResult<Token> {
    match_token(Ident, Color::Table)(i)
}

pub fn match_keyword(token_kind: TokenKind) -> impl Fn(Input) -> IResult<Token> {
    move |i| crate::parser::match_token(token_kind, Color::Keyword)(i)
}

pub fn match_bracket(token_kind: TokenKind) -> impl Fn(Input) -> IResult<Token> {
    move |i| crate::parser::match_token(token_kind, Color::Keyword)(i)
}
