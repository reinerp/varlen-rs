use varlen::define_varlen;

#[define_varlen]
/// This struct contains two lengths and three arrays.
pub struct CoolStruct {
    /// This length controls the first two arrays.
    #[header]
    pub small_len: u8,
    /// This length controls the last array.
    #[header]
    pub big_len: usize,

    #[varlen_array]
    /// An array.
    pub arr1: [u8; self.small_len as usize],

    #[varlen_array]
    /// Another array.
    pub arr2: [u8; self.small_len as usize],

    #[varlen_array]
    /// A third array.
    pub arr3: [u16; self.big_len],
}

#[define_varlen]
/// This struct contains two lengths and three arrays.
pub struct CoolStructWithMutableFields {
    /// This length controls the first two arrays.
    #[header]
    pub small_len: u8,
    /// This length controls the last array.
    #[header]
    pub big_len: usize,

    /// This field can be modified on an existing object.
    pub some_string: String,
    /// So can this.
    pub some_int: i16,

    #[varlen_array]
    /// An array.
    pub arr1: [u8; self.small_len as usize],

    #[varlen_array]
    /// Another array.
    pub arr2: [u8; self.small_len as usize],

    #[varlen_array]
    /// A third array.
    pub arr3: [u16; self.big_len],
}

#[cfg(test)]
mod tests {
    use varlen::define_varlen;
    use varlen::vbox::VBox;
    use varlen::init::FillSequentially;

    #[define_varlen]
    struct T {
        #[header]
        len: usize,

        #[varlen_array]
        arr: [u8; self.len],

        #[varlen_array]
        arr2: [u16; self.len * 4],
    }

    #[test]
    fn field_access_and_modify() {
        let mut b = VBox::<T>::new(t::Init{
            header: t::Header {
                len: 4,
            },
            arr: FillSequentially(|i| i as u8),
            arr2: FillSequentially(|i| i as u16),
        });
        assert_eq!(b.header.len, 4);
        assert_eq!(b.arr(), &[0, 1, 2, 3]);
        b.as_mut().arr_mut()[2] = 4;
        assert_eq!(b.arr(), &[0, 1, 4, 3]);
        assert_eq!(b.arr2(), &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]);
        b.as_mut().arr2_mut()[2] = 4;
        assert_eq!(b.arr2(), &[0, 1, 4, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]);
    }

    const MY_CONSTANT: usize = 4;
    type MyType = u16;

    #[define_varlen]
    pub struct VisibilityScenarios {
        #[header]
        len: usize,

        #[header]
        pub(self) len2: usize,

        #[header]
        pub(super) len3: usize,

        #[header]
        pub(crate) len4: usize,

        #[header]
        pub len5: usize,

        #[varlen_array]
        arr1: [MyType; MY_CONSTANT * self.len],
    }

    #[test]
    fn compile_errors_are_good() {
        let t = trybuild::TestCases::new();
        t.compile_fail("shouldfail/*.rs");
    }
}
