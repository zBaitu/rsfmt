fn f() {
    match a {
        <T as Trait>::Some() => true,
        Some(b) => true,
        Some(..) => true,
        Some(a, .., b) => true,
    }
}
