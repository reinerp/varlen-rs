use variable_length::define_varlen;

#[define_varlen]
struct UnnamedStruct(u8);

#[define_varlen]
enum Enum { A, B }

#[define_varlen]
union Union {
    a: u8,
    b: u16,
}

#[define_varlen]
pub struct UnsupportedVisibility {
    pub(in crate::foo::bar) x: u8,
}

#[define_varlen]
pub struct NotAnArray {
    #[varlen]
    x: u8,
}

#[define_varlen]
pub struct TooManyAttributes {
    #[varlen] #[header]
    x: [u8; 4],
}

#[define_varlen]
fn f() {}

fn main() {}