use std::path::PathBuf;

use structopt::StructOpt;

mod ast;
//mod ft;
mod ir;
mod rsfmt;
mod translator;
//mod ts;

#[derive(Debug, StructOpt)]
pub struct Opt {
    #[structopt(long, short)]
    /// Print the rust original syntax ast debug info
    ast: bool,

    #[structopt(long, short)]
    /// Check exceed lines and trailing white space lines
    check: bool,

    #[structopt(long, short)]
    /// Print the rsfmt ir debug info
    debug: bool,

    #[structopt(long, short)]
    /// Print the rsfmt ir simple format
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
        //rsfmt::fmt_from_stdin(opt);
    } else if opt.ast {
        rsfmt::dump_ast(&opt.input.unwrap());
    } else if opt.debug {
        rsfmt::debug(&opt.input.unwrap());
    } else if opt.print {
        //rsfmt::print(&opt.input.unwrap());
    } else {
        //rsfmt::fmt(opt);
    }
}
