unsafe fn very_long_unsafe_function(vec: &Vec<usize>) -> &Vec<usize> {
    vec
}

fn long_outer_function<'a, F>(index: usize, function: F) -> &'a usize
where F: Fn(usize) -> &'a usize,
{
    function(index)
}

fn main() {
    let vec = vec![1, 2, 3, 4];
    // Line which will be split up and reformatted by rust-fmt - compiles and runs without issues
    let output: Vec<_> = vec![0, 0, 1, 1, 2, 2].iter().map(|&index| long_outer_function(index, |index| &unsafe { very_long_unsafe_function(&vec) }[index])).collect();
    println!("{:?}", output);
}
