pub mod parser;
pub mod syntax;
pub mod token;

use std::borrow::Cow;
use std::io::stdout;
use std::io::Write;

use crossterm::execute;
use crossterm::style::Color;
use crossterm::style::Print;
use crossterm::style::ResetColor;
use crossterm::style::SetBackgroundColor;
use crossterm::style::SetForegroundColor;
use crossterm::terminal::Clear;
use crossterm::terminal::ClearType;
use crossterm::ExecutableCommand;
use rustyline::highlight::Highlighter;
use rustyline::Editor;
use rustyline_derive::Completer;
use rustyline_derive::Helper;
use rustyline_derive::Hinter;
use rustyline_derive::Validator;
