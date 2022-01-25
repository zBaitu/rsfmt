type a<> = bool;
type a<'a> = bool;
type a<T> = bool;
type a<'a, 'b: 'a, 'c: 'a + 'b> = bool;
type a<'a, T: 'a> = bool;
type a<T: Sized> = bool;
type b<T: ?Sized> = bool;

type a<T: for<'a> ::iter<bool>::Iterator<A, B, C, B = A, C: A  +  B> + Sized> = bool;
type a<T: Fn(A, B) -> ()> = bool;
type a<T = u8> = bool;

type a<const N: usize> = bool;
impl a for StaticVec<T, {N}> {}
