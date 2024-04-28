use highlight::parser::Input;
use highlight::syntax::query;
use highlight::syntax::select_stmt;
use highlight::token::Tokenizer;

const CASES: &[&str] = &[
    "",
    "select",
    "select ;",
    "1",
    "select a (1) from t;",
    "from t;",
    "select 1 1 1 1 1 1 1 1 ;",
];

#[divan::bench(args = CASES)]
fn fast_path(text: &str) {
    let tokens: Vec<highlight::token::Token<'_>> = Tokenizer::new(text).collect::<Vec<_>>();
    let input = Input {
        tokens: &tokens,
        fast_path: true,
    };
    divan::black_box(select_stmt(input));
}

#[divan::bench(args = CASES)]
fn rich_path(text: &str) {
    let tokens: Vec<highlight::token::Token<'_>> = Tokenizer::new(text).collect::<Vec<_>>();
    let input = Input {
        tokens: &tokens,
        fast_path: false,
    };
    divan::black_box(select_stmt(input));
}

fn main() {
    divan::main();
}
