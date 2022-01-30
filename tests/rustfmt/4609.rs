macro_rules! outer {
    ($d:tt) => {
        macro_rules! inner {
            ($d s:expr) => {
                println!("{}", $d s);
            }
        }
    };
}

outer!($);

fn main() {
    inner!("hi");
}
