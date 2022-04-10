use std::cell::RefCell;
use std::rc::{Rc, Weak};

#[derive(Debug)]
struct Node {
    value: i32,
    parent: RefCell<Weak<Node>>,
    children: RefCell<Vec<Rc<Node>>>,
}

fn main() {
    let leaf = Rc::new(Node {
        value: 3,
        parent: RefCell::new(Weak::new()),
        children: RefCell::new(vec![]),
    });

    println!("before parent assignment:");
    println!("  leaf parent: {:?}", leaf.parent.borrow().upgrade());

    let branch = Rc::new(Node {
        value: 5,
        parent: RefCell::new(Weak::new()),
        children: RefCell::new(vec![Rc::clone(&leaf)]),
    });

    *leaf.parent.borrow_mut() = Rc::downgrade(&branch);
    println!("after parent assignment:");
    println!("  leaf parent: {:?}", leaf.parent.borrow().upgrade());

    println!();
    println!("before drop:");
    println!(
        "  leaf - weak: {} strong: {}",
        Rc::weak_count(&leaf),
        Rc::strong_count(&leaf)
    );
    println!(
        "  branch - weak: {} strong: {}",
        Rc::weak_count(&branch),
        Rc::strong_count(&branch)
    );
    println!("after branch gets dropped:");
    std::mem::drop(branch);
    println!(
        "  leaf - weak: {} strong: {}",
        Rc::weak_count(&leaf),
        Rc::strong_count(&leaf)
    );
    println!("  leaf parent: {:?}", leaf.parent.borrow().upgrade());
}
