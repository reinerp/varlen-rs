use varlen::define_varlen;

const MY_CONST: usize = 4;

fn next_rand() -> usize {
    4
}

#[define_varlen]
struct S {
    #[header]
    u8_len: u8,

    not_a_header: u16,

    #[varlen_array]
    arr: [u8; x * y],

    #[varlen_array]
    arr2: [u8; self.u8_len],

    #[varlen_array]
    arr3: [u8; self.not_a_header],

    #[varlen_array]
    arr4: [u8; (&self) as *const _ as usize],

    #[varlen_array]
    arr5: [u8; (&self) as *const _ as usize],

    #[varlen_array]
    arr6: [u8; 4],

    #[varlen_array]
    arr7: [u8; next_rand()],

    #[varlen_array]
    arr8: [u8; MY_CONST],
}

fn main() {}