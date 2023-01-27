use crate::{
    Opt,
    formatter,
    translator::{self, TrResult},
};

use rustc_ap_rustc_ast::util::comments;
use rustc_ap_rustc_parse::{self as parse};
use rustc_ap_rustc_session::parse::ParseSess;
use rustc_ap_rustc_span::{
    self as span,
    FileName,
    edition::Edition,
    source_map::FilePathMapping,
};
use walkdir::WalkDir;

use std::{
    fs,
    fs::File,
    io::{self, Read, Write},
    panic,
    path::{Path, PathBuf},
};

macro_rules! p {
    ($arg:expr) => ({println!("{}", $arg)});
    ($fmt:expr, $($arg:tt)*) => ({println!($fmt, $($arg)*)});
}

macro_rules! ep {
    ($arg:expr) => ({eprintln!("{}", $arg)});
    ($fmt:expr, $($arg:tt)*) => ({eprintln!($fmt, $($arg)*)});
}

macro_rules! d {
    ($arg:expr) => ({println!("{:#?}", $arg)});
}

const SEP: &str = r#"
------------------------------------------------------------------------------------------------------------------------
"#;

pub fn ast(path: &Path) {
    let src = fs::read_to_string(path).unwrap();
    span::with_session_globals(Edition::Edition2021, || {
        let sess = ParseSess::new(FilePathMapping::empty());
        let krate = match parse::parse_crate_from_source_str(FileName::from(PathBuf::from(path)), src.clone(), &sess) {
            Ok(krate) => krate,
            Err(mut e) => {
                e.emit();
                std::process::exit(1);
            },
        };
        d!(krate);

        p!(SEP);

        let cmnts = comments::gather_comments(sess.source_map(), FileName::from(PathBuf::from(path)), src);
        for cmnt in cmnts {
            p!("{}: {:#?} {:#?}", cmnt.pos.0, cmnt.style, cmnt.lines);
        }
    });
}

pub fn debug(path: &Path) {
    let src = fs::read_to_string(path).unwrap();
    let result = trans(src, path);

    d!(result.krate);
    p!(SEP);
    d!(result.leading_cmnts);
    d!(result.trailing_cmnts);
}

pub fn print(path: &Path) {
    let src = fs::read_to_string(path).unwrap();
    let result = trans(src, path);
    p!(result.krate);
}

pub fn fmt_from_stdin(opt: &Opt) {
    let mut src = String::new();
    io::stdin().read_to_string(&mut src).unwrap();
    try_fmt_str(src, &PathBuf::from("stdin"), opt);
}

pub fn fmt(path: &Path, opt: &Opt) {
    if path.is_dir() {
        fmt_dir(path, opt);
    } else {
        fmt_file(path, opt);
    }
}

fn fmt_dir(path: &Path, opt: &Opt) {
    const SKIP_DIRS: &[&str] = &[".git", "target"];

    let dir = WalkDir::new(path);
    for entry in dir.into_iter().filter_entry(|e| !SKIP_DIRS.contains(&e.file_name().to_str().unwrap())) {
        let entry = entry.unwrap();
        if entry.file_type().is_file() {
            let path = entry.into_path();
            if let Some(ext) = path.extension() {
                if ext == "rs" {
                    fmt_file(&path, opt);
                }
            }
        }
    }
}

fn fmt_file(path: &Path, opt: &Opt) {
    let src = fs::read_to_string(path).unwrap();
    try_fmt_str(src, path, opt);
}

fn try_fmt_str(src: String, path: &Path, opt: &Opt) {
    let result = panic::catch_unwind(|| {
        fmt_str(src, path, opt);
    });

    if let Err(err) = result {
        ep!(SEP);
        ep!(path.display());
        panic!("{:?}", err);
    }
}

fn fmt_str(src: String, path: &Path, opt: &Opt) {
    if opt.check {
        p!(path.display());
    }

    let tr_result = trans(src, path);
    let ft_result = formatter::format(tr_result.krate, tr_result.leading_cmnts, tr_result.trailing_cmnts);

    if opt.overwrite {
        let mut file = File::create(path).unwrap();
        file.write_all(ft_result.s.as_bytes()).unwrap();
    } else if !opt.check {
        p!(ft_result.s);
    }

    let mut exit = false;
    if !ft_result.exceed_lines.is_empty() {
        ep!("exceed_lines: {:?}", ft_result.exceed_lines);
        exit = true;
    }
    if !ft_result.trailing_ws_lines.is_empty() {
        ep!("trailing_ws_lines: {:?}", ft_result.trailing_ws_lines);
        exit = true;
    }
    if exit {
        ep!("{}", path.display());
    }
    if exit && !opt.keep {
        if !opt.check {
            ep!(path.display());
        }
        std::process::exit(1);
    }
}

fn trans(src: String, path: &Path) -> TrResult {
    span::with_session_globals(Edition::Edition2021, || {
        let sess = ParseSess::new(FilePathMapping::empty());
        let krate = parse::parse_crate_from_source_str(FileName::from(path.to_path_buf()), src.clone(), &sess).unwrap();
        let cmnts = comments::gather_comments(sess.source_map(), FileName::from(path.to_path_buf()), src.clone());
        translator::translate(src, sess, krate, cmnts)
    })
}
