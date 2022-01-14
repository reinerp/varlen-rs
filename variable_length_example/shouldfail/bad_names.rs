use variable_length::define_varlen;

#[define_varlen]
struct StructWithBadNames  {
    #[header]
    x: u8,

    #[header]
    x: u8,
}

fn main() {}