default impl A {}

//unsafe impl !A {}

impl A for B {}

impl<T> A for B<T> {
    default const a: bool = true;
    type E = T;
    fn f(&self) {}
    //a!(true);
}

