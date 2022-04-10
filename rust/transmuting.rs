fn main() {
    let a: u16 = 50115;
    let ba: i16 = unsafe { std::mem::transmute(a) };
    println!("ba: {:016b} {}", ba, ba);

    let from_float: u32 = unsafe { std::mem::transmute(1f32) };
    println!("from_float: {:032b} {}", from_float, from_float);
    let from_float: u32 = unsafe { std::mem::transmute(-1f32) };
    println!("from_float: {:032b} {}", from_float, from_float);

    let big: [u8; 4] = [0, 0, 0, 1];
    let little: [u8; 4] = [1, 0, 0, 0];

    let v: u32 = unsafe { std::mem::transmute(big) };
    println!("big:    {:032b} ({})", v, v);
    let v: u32 = unsafe { std::mem::transmute(little) };
    println!("little: {:032b} ({})", v, v);
}
