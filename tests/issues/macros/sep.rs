radix! { Binary,    2, "0b", x @  0 ..=  1 => b'0' + x }

right_arrow! {NonZeroI8(i8) -> NonZeroU8(u8);}

fn f() {
    matches!(a, b if c < d);

    matches!(*self, '!'..='/'   |   ':'..='@');

    test!{0 1 2  4
    }
}

