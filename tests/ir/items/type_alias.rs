type a;
type a = _;
type a = !;
type a = bool;
type a = result::Result;
type a = <::i32 as Vec<bool>>::MAX;
type a = [bool];
type a = [[bool]];
type a = [bool; 8];
type a = *const bool;
type a = *mut bool;
type a = &bool;
type a = &'a mut bool;
type a = &[bool];
type a = &'a bool;
type a = ();
type a = (bool);
type a = (bool, usize);
type a = _;
type a = unsafe extern "C" fn(bool) -> usize;
type a = dyn Result + Iterator<T> + 'static + Sized;
type a = impl Result + Iterator<T> + 'static + Sized;
type a = for<'a, 'b: 'a> Foo<&'a Bar>;
type a = a!("a");
type a<T>  where T: Iterator = Option<T>;
