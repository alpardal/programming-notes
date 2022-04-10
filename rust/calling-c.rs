use std::io;

fn main() {
    syscall("Hello from C syscall binding\n").unwrap();
}

#[cfg(not(target_os = "windows"))]
#[link(name = "c")]
extern "C" {
    fn write(fd: u32, buf: *const u8, count: usize) -> i32;
}

#[cfg(not(target_os = "windows"))]
fn syscall(msg: &str) -> io::Result<()> {
    let msg_ptr = msg.as_ptr();
    let len = msg.len();
    let res = unsafe { write(1, msg_ptr, len) };

    if res == -1 {
        Err(io::Error::last_os_error())
    } else {
        Ok(())
    }
}
