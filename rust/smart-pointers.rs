// cells provide mutability from non &mut refs,
// while keeping statically enforced ownership rules
mod _cells {
    use std::cell::Cell;
    use std::fmt::Debug;

    #[derive(Debug)]
    struct Bla {
        data: Cell<usize>,
    }

    fn work(bla: &Bla) {
        let was = bla.data.replace(42);
        println!("was: {} now: {}", was, bla.data.get());
    }

    pub fn test() {
        let bla = Bla {
            data: Cell::new(10),
        };
        work(&bla);
        // since ownership rules are kept, the following won't compile:
        //   let another = bla;
        //   println!("{:?}", &bla);
    }
}

// ref cells check borrow rules at *run time*
// attempting multiple mut borrows will panic instead of causing compile errors
mod _ref_cells {
    use std::cell::RefCell;

    struct Data {
        val: RefCell<usize>,
    }

    fn work(data: &Data) {
        *data.val.borrow_mut() = 42;
    }

    pub fn test() {
        let data = Data {
            val: RefCell::new(10),
        };
        println!("before: {}", data.val.borrow());
        // works: no existing borrow:
        work(&data);
        // works: mut borrow in `work` has already been dropped
        let a = data.val.borrow();
        println!("after: {}", &a);
        // works: still no mut borrows in existance
        let b = data.val.borrow();
        println!("after: {}", &b);
        // the line below would panic since a and b are still in scope
        // let c = data.val.borrow_mut();
    }
}

pub fn test() {
    println!(" -- Cells --");
    _cells::test();
    println!(" -- RefCells --");
    _ref_cells::test();
}
