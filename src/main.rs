use structopt::StructOpt;

use std::path::PathBuf;

mod ast;
mod formatter;
mod ir;
mod rsfmt;
mod translator;
mod typesetter;

#[derive(Debug, StructOpt)]
pub struct Opt {
    #[structopt(long, short)]
    /// Prints the rust original syntax ast debug info
    ast: bool,

    #[structopt(long, short)]
    /// Only check without output or overwrite
    check: bool,

    #[structopt(long, short)]
    /// Prints the rsfmt ir debug info
    debug: bool,

    #[structopt(long, short)]
    /// Keep going when error occurred
    keep: bool,

    #[structopt(long, short)]
    /// Prints the rsfmt ir simple format
    print: bool,

    #[structopt(long, short)]
    /// Overwrite the source file
    overwrite: bool,

    /// Input file or dir.
    /// If `input` is a dir, rsfmt will do action for all files in this dir recursively.
    /// If neither `options` nor `input` is specified, rsfmt will format source code from stdin.
    #[structopt(parse(from_os_str))]
    input: Option<PathBuf>,
}

fn main() {
    let opt = Opt::from_args();
    if opt.input.is_none() {
        rsfmt::fmt_from_stdin(&opt);
        return;
    }

    let path = opt.input.as_ref().unwrap();
    if opt.ast {
        rsfmt::ast(path);
    } else if opt.debug {
        rsfmt::debug(path);
    } else if opt.print {
        rsfmt::print(path);
    } else {
        rsfmt::fmt(path, &opt);
    }
}
