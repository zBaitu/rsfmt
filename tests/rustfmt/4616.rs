#[derive(Debug)]
struct Foo {
  some:   usize,
  member: String,
}

impl Foo {
  pub fn some_fn() {
    let edb_file = fs::read_dir(&event_path)?.find_map(|p| {
      let path = p.ok()?.path();
      (path.is_file() && Self::as_regex().is_match(&get_file_name(&path).ok()?) && path.extension()? == "edb").as_some(path)
    });
  }
}
