use variable_length::define_varlen;

#[define_varlen]
struct StructWithUndefinedTypes  {
    #[header]
    header_variable: UndefinedType1,

    mut_variable: UndefinedType2,

    #[varlen]
    arr: [UndefinedType3; 4usize],
}

struct RequiresCopy<T: Copy>(T);

#[define_varlen]
struct StructWithIllFormedTypes  {
    #[header]
    header_variable: RequiresCopy<String>,

    mut_variable: RequiresCopy<Vec<String>>,

    #[varlen]
    arr: [RequiresCopy<Vec<Vec<String>>>; 4usize],
}

struct StructWithUnsizedTypes {
    #[header]
    header_variable: [u8],

    mut_variable: [u16],

    #[varlen]
    arr: [[u32]; 4],
}



fn main() {}