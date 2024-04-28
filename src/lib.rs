pub mod parser;
pub mod syntax;
pub mod token;

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
