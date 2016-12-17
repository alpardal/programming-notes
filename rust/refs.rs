// https://github.com/rust-lang/rust-by-example/issues/390

fn main() {
    let x = 3;

    let a = &x;
    // same as:
    let ref a = x;

    let s1 = Some(&x);
    match s1 {
        // here y = x (it's destructured the same way it's constructed)
        Some(&y) => {}
        None => {}
    }

    let s2 = Some(x);
    match s2 {
        // here y = &x
        Some(ref y) => {}
        None => {}
    }
}

