mod parser;
mod syntax;
mod token;

use crossterm::{
    execute,
    style::{Color, Print, ResetColor, SetBackgroundColor, SetForegroundColor},
    terminal::{Clear, ClearType},
    ExecutableCommand,
};
use rustyline::{highlight::Highlighter, Editor};
use rustyline_derive::{Completer, Helper, Hinter, Validator};
use std::{
    borrow::Cow,
    io::{stdout, Write},
};

#[derive(Completer, Helper, Hinter, Validator)]
struct SyntaxHighlighter {}

impl Highlighter for SyntaxHighlighter {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        let sematics = syntax::query(line);

        let mut output = vec![];
        for (i, ch) in line.char_indices() {
            let sematic = sematics
                .iter()
                .find(|(span, _)| span.start <= i && i < span.end)
                .map(|(_, sematic)| sematic);
            if let Some(sematic) = sematic {
                output
                    .execute(SetForegroundColor(sematic.foreground_color()))
                    .unwrap()
                    .execute(SetBackgroundColor(sematic.backgrourd_color()))
                    .unwrap()
                    .execute(Print(ch))
                    .unwrap()
                    .execute(ResetColor)
                    .unwrap();
            } else {
                write!(&mut output, "{}", ch).unwrap();
            }
        }

        Cow::Owned(String::from_utf8(output).unwrap())
    }

    fn highlight_char(&self, _line: &str, _pos: usize, _forced: bool) -> bool {
        true
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut rl = Editor::new()?;
    rl.set_helper(Some(SyntaxHighlighter {}));
    loop {
        match rl.readline(">> ") {
            Ok(_) => {}
            Err(rustyline::error::ReadlineError::Interrupted)
            | Err(rustyline::error::ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    Ok(())
}
