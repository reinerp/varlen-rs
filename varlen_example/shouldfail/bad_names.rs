use varlen::define_varlen;

#[define_varlen]
struct StructWithBadNames  {
    #[controls_layout]
    x: u8,

    #[controls_layout]
    x: u8,
}

fn main() {}