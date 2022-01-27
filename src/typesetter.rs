use std::collections::BTreeSet;
use std::fmt::{self, Debug};

const NL: char = '\n';

const EXCEED_WIDTH: usize = 120;
const MAX_WIDTH: usize = EXCEED_WIDTH - 2;
const MAX_ALIGN_COL: usize = EXCEED_WIDTH / 3;

const INDENT: &str = "    ";
const INDENT_LEN: usize = INDENT.len();
const WRAP_INDENT: &str = "        ";
const WRAP_INDENT_LEN: usize = WRAP_INDENT.len();

#[macro_export]
macro_rules! need_wrap {
    ($ts:expr, $($s:expr),+) => ({
        $ts.need_wrap(&[$($s),+])
    });
}

#[macro_export]
macro_rules! need_nl_indent {
    ($ts:expr, $($s:expr),+) => ({
        $ts.need_nl_indent(&[$($s),+])
    });
}

macro_rules! raw_insert {
    ($sf:expr, $s:expr) => ({
        $sf.s.push_str($s);

        $sf.col += $s.len();
        if $sf.col > EXCEED_WIDTH {
            $sf.exceed_lines.insert($sf.line);
        }
    });
}

macro_rules! minus_novf {
    ($a: expr, $b: expr) => ({
        if $a <= $b {
            0
        } else {
            $a - $b
        }
    });
}

fn list_len_info(list: &[&str]) -> (usize, usize) {
    let mut prefix_len = 0;
    let len = list.iter().map(|s| str_one_line_len(s)).sum();
    if list.len() > 1 {
        prefix_len = len - str_one_line_len(list.last().unwrap());
    }
    (prefix_len, len)
}

fn str_one_line_len(s: &str) -> usize {
    if let Some(pos) = s.find('\n') {
        pos
    } else {
        s.len()
    }
}

#[derive(Default)]
pub struct Typesetter {
    line: u32,
    col: usize,
    indent: String,
    align_stack: Vec<usize>,

    s: String,
    exceed_lines: BTreeSet<u32>,
    trailing_ws_lines: BTreeSet<u32>,
}

pub struct TsResult {
    pub s: String,
    pub exceed_lines: BTreeSet<u32>,
    pub trailing_ws_lines: BTreeSet<u32>,
}

impl Debug for Typesetter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "pos: ({}, {})\n", self.line, self.col)?;
        writeln!(f, "indent: \"{}\"\n", self.indent)?;
        write!(f, "align stack: ")?;
        Debug::fmt(&self.align_stack, f)?;
        write!(f, "\nexceed lines: ")?;
        Debug::fmt(&self.exceed_lines, f)
    }
}

impl Typesetter {
    pub fn new() -> Typesetter {
        Typesetter {
            line: 1,
            ..Default::default()
        }
    }

    pub fn result(self) -> TsResult {
        TsResult {
            s: self.s,
            exceed_lines: self.exceed_lines,
            trailing_ws_lines: self.trailing_ws_lines,
        }
    }

    pub fn force_insert(&mut self, s: &str) {
        self.s.push_str(s);
    }

    pub fn raw_insert(&mut self, s: &str) {
        raw_insert!(self, s);
    }

    pub fn insert(&mut self, s: &str) {
        if need_wrap!(self, s) {
            self.wrap_insert(s);
        } else {
            self.raw_insert(s);
        }
    }

    pub fn indent(&mut self) {
        self.indent.push_str(INDENT);
    }

    pub fn outdent(&mut self) {
        let len = self.indent.len();
        self.indent.truncate(len - INDENT_LEN);
    }

    pub fn insert_indent(&mut self) {
        raw_insert!(self, &self.indent);
    }

    pub fn nl(&mut self) {
        if let Some(ch) = self.s.chars().last() {
            if ch != NL && ch.is_whitespace() {
                self.trailing_ws_lines.insert(self.line);
            }
        }

        self.s.push(NL);
        self.line += 1;
        self.col = 0;
    }

    pub fn nl_indent(&mut self) {
        self.nl();
        self.insert_indent();
    }

    pub fn can_one_line(&self, s: &str) -> bool {
        self.left() > s.len()
    }

    pub fn need_wrap(&self, list: &[&str]) -> bool {
        let (prefix_len, len) = list_len_info(list);
        self.need_wrap_len(prefix_len, len)
    }

    pub fn need_nl_indent(&self, list: &[&str]) -> bool {
        let (prefix_len, len) = list_len_info(list);
        self.need_nl_indent_len(prefix_len, len)
    }

    pub fn wrap(&mut self) {
        self.nl();

        if self.should_align() {
            self.insert_align();
        } else {
            self.insert_indent();
            self.insert_wrap_indent();
        }
    }

    pub fn insert_mark_align(&mut self, s: &str) {
        self.raw_insert(s);
        self.mark_align();
    }

    pub fn insert_unmark_align(&mut self, s: &str) {
        self.raw_insert(s);
        self.unmark_align();
    }

    fn wrap_insert(&mut self, s: &str) {
        self.wrap();
        self.raw_insert(s);
    }

    fn need_wrap_len(&self, prefix_len: usize, len: usize) -> bool {
        self.left() <= prefix_len || (self.left() < len && len <= self.nl_left())
    }

    fn need_nl_indent_len(&self, prefix_len: usize, len: usize) -> bool {
        self.left() <= prefix_len || (self.left() < len && len <= self.nl_indent_left())
    }

    fn nl_left(&self) -> usize {
        if self.should_align() {
            self.nl_align_left()
        } else {
            self.nl_wrap_left()
        }
    }

    fn should_align(&self) -> bool {
        matches!(self.align_stack.last(), Some(col) if *col <= MAX_ALIGN_COL)
    }

    fn nl_align_left(&self) -> usize {
        minus_novf!(MAX_WIDTH, *self.align_stack.last().unwrap())
    }

    fn nl_wrap_left(&self) -> usize {
        minus_novf!(MAX_WIDTH, self.indent.len() + WRAP_INDENT_LEN)
    }

    fn nl_indent_left(&self) -> usize {
        minus_novf!(MAX_WIDTH, self.indent.len())
    }

    fn left(&self) -> usize {
        minus_novf!(MAX_WIDTH, self.col)
    }

    fn insert_align(&mut self) {
        let blank = " ".repeat(*self.align_stack.last().unwrap());
        self.raw_insert(&blank);
    }

    fn insert_wrap_indent(&mut self) {
        self.raw_insert(WRAP_INDENT);
    }

    fn mark_align(&mut self) {
        self.align_stack.push(self.col);
    }

    fn unmark_align(&mut self) {
        self.align_stack.pop();
    }
}
