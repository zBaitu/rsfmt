use *;
use ::*;
use ::f;
use a::b::{c, d};
use a::b::{c::{d::{e::{f::g}}}};
use a::b::{c, d, e::f, 
    g::h::i};
use a::b::{self, c, d::e};
use a::b::{self as ab, c as abc};
use a::b::*;
use a::b::{
    self as ab, c, g, h,
    d::{*, e::f},
};
use a::b::{
    self as ab, c, g, h,
    d::{*, 
        e::f},
};

use a::b::{ self as ab, 
    c, g, h, d::{*, e::f} };
use p::q::r as x;

use crate::aa as x;

