static TLS_DESTRUCTOR: [AtomicUsize; TLS_KEYS] = dup!((* * * * * * *) (AtomicUsize::new(0)));
