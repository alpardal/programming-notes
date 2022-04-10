fn main() {
    let mut x = 1;
    println!("before: {}", x);

    let y = &mut x;
    // doesn't compile: y is moved into z, an so it cannot be used
    // again down below
    // let z = y;

    // fine: reborrows
    let z: &mut isize = y;
    // same as:
    // let z = &mut * y;

    increment(z);
    increment(y);
    // function calls also use reborrowing, so y can be used twice:
    increment(y);
    // TODO: investigate reborrowing in fn calls and wrt generics

    println!("after: {}", x);
}

fn increment(val: &mut isize) {
    *val += 1;
}
