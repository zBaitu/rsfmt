async unsafe fn f() {}

const fn f() {}

extern "C" fn f() {}

pub fn fmt(krate: Crate, leading_cmnts: HashMap<Pos, Vec<String>>,
           trailing_cmnts: HashMap<Pos, String>) -> rfmt::Result {
    Formatter::new(leading_cmnts, trailing_cmnts).fmt_crate(krate)
}

fn f() -> bool { true }
fn f() -> bool { 
    true }

fn func_where<T>() -> bool 
where T: ?Sized{}

fn func_where<T>()
    -> bool where T: ?Sized{}
