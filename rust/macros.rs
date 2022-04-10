// simple substitution:
macro_rules! two_usize {
    ($name:ident) => {
        let $name = 2_usize;
    };
}

// substitution w/ multi expansion
macro_rules! usize_struct_with_meta {
    ($(#[$info:meta])* $name:ident) => {
        $(#[$info])*
        struct $name(usize);
    };
}

usize_struct_with_meta![
    #[derive(Default)]
    Data
];

// literal matching
macro_rules! simple {
    (type $name:ident) => {
        struct $name();
    };
}

simple![type Person];

fn main() {
    two_usize!(_bla);
    let _data = Data::default();
    let _p = Person();
}
