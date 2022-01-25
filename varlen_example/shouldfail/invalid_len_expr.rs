use varlen::define_varlen;

#[define_varlen]
struct S {
    #[controls_layout]
    u8_len: u8,

    not_a_header: u16,

    #[varlen_array]
    arr: [u8; x * y],

    #[varlen_array]
    arr2: [u8; *u8_len],

    #[varlen_array]
    arr3: [u8; *not_a_header],

    #[varlen_array]
    arr6: [u8; 4],
}

fn main() {}