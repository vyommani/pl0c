pub trait TargetOS {
    fn syscall_write(&self) -> u32;
    fn syscall_read(&self) -> u32;
    fn syscall_exit(&self) -> u32;
    fn clone_box(&self) -> Box<dyn TargetOS>;
}

pub struct Linux;
impl TargetOS for Linux {
    fn syscall_write(&self) -> u32 { 1 }
    fn syscall_read(&self) -> u32 { 0 }
    fn syscall_exit(&self) -> u32 { 60 }
    fn clone_box(&self) -> Box<dyn TargetOS> {
        Box::new(Linux)
    }
}

pub struct FreeBSD;
impl TargetOS for FreeBSD {
    fn syscall_write(&self) -> u32 { 4 }
    fn syscall_read(&self) -> u32 { 3 }
    fn syscall_exit(&self) -> u32 { 1 }
    fn clone_box(&self) -> Box<dyn TargetOS> {
        Box::new(FreeBSD)
    }
}
pub struct MacOS;
impl TargetOS for MacOS {
    fn syscall_write(&self) -> u32 { 0x2000004 }  // macOS syscall numbers have 0x2000000 offset
    fn syscall_read(&self) -> u32 { 0x2000003 }
    fn syscall_exit(&self) -> u32 { 0x2000001 }
    fn clone_box(&self) -> Box<dyn TargetOS> {
        Box::new(MacOS)
    }
}