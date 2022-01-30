macro_rules! test_arrow {
    ($x:expr => $y:expr) => {
        println!("{} {}", $x, $y);
    };
}

macro_rules! test_comma {
    ($x:expr, $y:expr) => {
        println!("{} {}", $x, $y);
    };
}

fn main() {
    test_arrow!("hello" =>   2    + 2);
    test_comma!("hello", 2 + 2);
}
