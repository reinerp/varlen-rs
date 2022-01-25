use varlen::define_varlen;

const MY_CONST: usize = 4;

fn next_rand() -> usize {
    4
}

#[define_varlen]
struct S {
    #[controls_layout]
    u8_len: u8,

    not_a_header: u16,

    #[varlen_array]
    arr1: [u8; u8_len as *const _ as usize],

    #[varlen_array]
    arr2: [u8; next_rand()],

    #[varlen_array]
    arr3: [u8; MY_CONST],

    #[varlen_array]
    arr4: [u8; std::mem::transmute(u8_len) ]
}

fn main() {}