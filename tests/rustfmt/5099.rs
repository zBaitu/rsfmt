fn                                                      main() {
    let   x    =    512_u16;
    let   byte    =  (  x  % 10   )    as  u8  /* make it printable */   +    b'0'        ;
    dbg!(      char::from(              
          byte));
}
