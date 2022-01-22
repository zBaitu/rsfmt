use std::fs;
use std::fs::File;
use std::io::{self, Read, Write};
use std::path::{PathBuf};

use rustc_ap_rustc_ast::util::comments;
use rustc_ap_rustc_parse::{self as parse};
use rustc_ap_rustc_session::parse::ParseSess;
use rustc_ap_rustc_span::{self as span, FileName, edition::Edition, source_map::FilePathMapping};
use walkdir::WalkDir;

use crate::formatter;
use crate::translator::{self, TrResult};

macro_rules! p {
    () => ({println!()});
    ($arg:expr) => ({println!("{}", $arg)});
    ($fmt:expr, $($arg:tt)*) => ({println!($fmt, $($arg)*)});
    ($($arg:tt)+) => ({println!("{}", $($arg)+)});
}

macro_rules! d {
    ($arg:expr) => ({println!("{:#?}", $arg)});
}

const SEP: &str = r#"
------------------------------------------------------------------------------------------------------------------------
"#;

pub fn dump_ast(path: &PathBuf) {
    let src = fs::read_to_string(path).unwrap();
    span::with_session_globals(Edition::Edition2021, || {
        let sess = ParseSess::new(FilePathMapping::empty());
        let krate = match parse::parse_crate_from_source_str(FileName::from(path.clone()), src.clone(), &sess) {
            Ok(krate) => krate,
            Err(mut e) => {
                e.emit();
                std::process::exit(1);
            },
        };
        d!(krate);

        p!(SEP);

        let cmnts = comments::gather_comments(&sess.source_map(), FileName::from(path.clone()), src);
        for cmnt in cmnts {
            p!("{}: {:#?} {:#?}", cmnt.pos.0, cmnt.style, cmnt.lines);
        }
    });
}

pub fn debug(path: &PathBuf) {
    let src = fs::read_to_string(path).unwrap();
    let result = trans(src, path);

    d!(result.krate);
    p!(SEP);
    d!(result.leading_cmnts);
    d!(result.trailing_cmnts);
}

pub fn print(path: &PathBuf) {
    let src = fs::read_to_string(path).unwrap();
    let result = trans(src, path);
    p!(result.krate);
}

pub fn fmt_from_stdin() {
    let mut src = String::new();
    io::stdin().read_to_string(&mut src).unwrap();
    fmt_str(src, &PathBuf::from("stdin"), false);
}

pub fn fmt(path: &PathBuf, overwrite: bool) {
    if path.is_dir() {
        fmt_dir(path, overwrite);
    } else {
        fmt_file(path, overwrite);
    }
}

fn fmt_dir(path: &PathBuf, overwrite: bool) {
    for entry in WalkDir::new(path) {
        let entry = entry.unwrap();
        if entry.file_type().is_file() {
            let path = entry.into_path();
            let ext = path.extension();
            if let Some(ext) = ext {
                if ext == "rs" {
                    fmt_file(&path, overwrite);
                }
            }
        }
    }
}

fn fmt_file(path: &PathBuf, overwrite: bool) {
    let src = fs::read_to_string(path).unwrap();
    fmt_str(src, path, overwrite);
}

fn fmt_str(src: String, path: &PathBuf, overwrite: bool) {
    let tr_result = trans(src, path);
    let ft_result = formatter::format(tr_result.krate, tr_result.leading_cmnts, tr_result.trailing_cmnts);

    if !ft_result.exceed_lines.is_empty() || !ft_result.trailing_ws_lines.is_empty() {
        p!("{:?}", path);
        if !ft_result.exceed_lines.is_empty() {
            p!("exceed_lines: {:?}", ft_result.exceed_lines);
        }
        if !ft_result.trailing_ws_lines.is_empty() {
            p!("trailing_ws_lines: {:?}", ft_result.trailing_ws_lines);
        }
        std::process::exit(1);
    }

    if overwrite {
        let mut file = File::create(path).unwrap();
        file.write_all(ft_result.s.as_bytes()).unwrap();
    } else {
        p!(ft_result.s);
    }
}

fn trans(src: String, path: &PathBuf) -> TrResult {
    span::with_session_globals(Edition::Edition2021, || {
        let sess = ParseSess::new(FilePathMapping::empty());
        let krate = parse::parse_crate_from_source_str(FileName::from(path.to_path_buf()), src.clone(), &sess).unwrap();
        let cmnts = comments::gather_comments(&sess.source_map(), FileName::from(path.to_path_buf()), src.clone());
        translator::translate(src, sess, krate, cmnts)
    })
}
