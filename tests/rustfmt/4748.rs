fn foo(x: Option<i32>) -> i32 {
    let y = {
        ;
        x
    };
    match y {
        Some(y) => {
            ;
            ;
            y
        }
        None => 0,
    }
}
