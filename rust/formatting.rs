fn binary_numbers() {
    let a: u16 = 50115;
    let b: i16 = -15421;
    println!("a: {:016b} {}", a, a);
    println!("b: {:016b} {}", b, b);
}

#[derive(Debug)]
struct Point2 {
    x: isize,
    y: isize,
}

fn pretty_formatting() {
    println!("{:?}", Point2 { x: 1, y: 2 });
    println!("{:#?}", Point2 { x: 1, y: 2 });
}

fn pointers() {
    let a = &42;
    println!("a: {:p}", a);
}
