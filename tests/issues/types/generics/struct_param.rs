fn f() {
    Box::new(Sum::<T> {
        ty,
        sum: T::default_value(),
        null: true,
    })
}
