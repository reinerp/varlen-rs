use variable_length::define_varlen;

const MY_CONST: usize = 4;

fn next_rand() -> usize {
    4
}

#[define_varlen]
struct S {
    #[header]
    u8_len: u8,

    not_a_header: u16,

    #[varlen]
    arr1: [u8; (&self) as *const _ as usize],

    #[varlen]
    arr2: [u8; next_rand()],

    #[varlen]
    arr3: [u8; MY_CONST],

    #[varlen]
    arr4: [u8; std::mem::transmute(&self) ]
}

fn main() {}