macro_rules! example_macro {
    ($r#example: tt) => {
        $r#example
    };
}

fn main() {
    println!(example_macro!("Hello, world!"));
}
