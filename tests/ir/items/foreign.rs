extern { type bool; type a = result::Result;}

extern { a!(true); }

extern "C" {
    static mut a: bool;
    pub fn f<T>(a: bool) -> i32;
    pub fn f<T>(a: bool) -> i32 {}
}

extern "Rust" {
    static mut a: bool;
}
