fn call1<F: FnOnce()>(f: F) {
    f();
}

// same as call
fn call2(f: impl FnOnce()) {
    f();
}

fn call3(f: Box<dyn FnOnce()>) {
    f();
}

fn main() {
    call1(|| println!("call"));
    call2(|| println!("call2"));
    call3(Box::new(|| println!("call3")));
}
