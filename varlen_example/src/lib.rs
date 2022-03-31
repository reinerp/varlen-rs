#![allow(unused)]
#![allow(dead_code)]
use varlen::prelude::*;

#[define_varlen]
/// This struct contains two lengths and three arrays.
pub struct CoolStruct {
    /// This length controls the first two arrays.
    #[controls_layout]
    pub small_len: u8,
    /// This length controls the last array.
    #[controls_layout]
    pub big_len: usize,

    #[varlen_array]
    /// An array.
    pub arr1: [u8; *small_len as usize],

    #[varlen_array]
    /// Another array.
    pub arr2: [u8; *small_len as usize],

    #[varlen_array]
    /// A third array.
    pub arr3: [u16; *big_len],
}

#[define_varlen]
/// This struct contains two lengths and three arrays.
pub struct CoolStructWithMutableFields {
    /// This length controls the first two arrays.
    #[controls_layout]
    pub small_len: u8,
    /// This length controls the last array.
    #[controls_layout]
    pub big_len: usize,

    /// This field can be modified on an existing object.
    pub some_string: String,
    /// So can this.
    pub some_int: i16,

    #[varlen_array]
    /// An array.
    pub arr1: [u8; *small_len as usize],

    #[varlen_array]
    /// Another array.
    pub arr2: [u8; *small_len as usize],

    #[varlen_array]
    /// A third array.
    pub arr3: [u16; *big_len],
}

#[cfg(test)]
mod tests {
    use varlen::prelude::*;
    use memoffset::offset_of;

    #[define_varlen]
    struct T {
        #[controls_layout]
        len: usize,

        #[varlen_array]
        arr: [u8; *len],

        #[varlen_array]
        arr2: [u16; *len * 4],
    }

    #[test]
    fn field_access_and_modify() {
        let mut b = VBox::<T>::new(t::Init {
            len: 4,
            arr: FillSequentially(|i| i as u8),
            arr2: FillSequentially(|i| i as u16),
        });
        assert_eq!(b.len, 4);
        assert_eq!(b.refs().arr, &[0, 1, 2, 3]);
        b.as_mut().muts().arr[2] = 4;
        assert_eq!(b.refs().arr, &[0, 1, 4, 3]);
        assert_eq!(
            b.refs().arr2,
            &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
        );
        b.as_mut().muts().arr2[2] = 4;
        assert_eq!(
            b.refs().arr2,
            &[0, 1, 4, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
        );
    }

    // #[define_varlen]
    // pub struct HasNest {
    //     #[allow(dead_code)]
    //     x: u32,

    //     #[varlen]
    //     nest: T,
    // }

    const MY_CONSTANT: usize = 4;
    type MyType = u16;

    #[define_varlen]
    pub struct VisibilityScenarios {
        #[controls_layout]
        len: usize,

        #[controls_layout]
        pub(self) len2: usize,

        #[controls_layout]
        pub(super) len3: usize,

        #[controls_layout]
        pub(crate) len4: usize,

        #[controls_layout]
        pub len5: usize,

        #[varlen_array]
        arr1: [MyType; MY_CONSTANT * *len],
    }

    #[define_varlen]
    pub struct PartialModifyLayout {
        #[controls_layout]
        header: u64,

        #[varlen_array]
        arr: [u8; (*header % 4) as usize],
    }

    #[test]
    fn modify_header_field_checked() {
        let mut b = VBox::<PartialModifyLayout>::new(partial_modify_layout::Init {
            header: (123 * 4) + 2,
            arr: MoveFrom([4, 5]),
        });
        assert_eq!(*b.refs().header, (123 * 4) + 2);
        assert_eq!(b.refs().arr, &[4, 5]);
        let header = b.header;
        b.as_mut()
            .try_set_layout_controllers(partial_modify_layout::LayoutControllers {
                header: header + 5 * 4,
            })
            .unwrap();
        assert_eq!(*b.refs().header, (128 * 4) + 2);
        assert_eq!(b.refs().arr, &[4, 5]);
    }

    #[test]
    fn modify_header_field_bad_layout_panics() {
        let mut b = VBox::<PartialModifyLayout>::new(partial_modify_layout::Init {
            header: (123 * 4) + 2,
            arr: MoveFrom([4, 5]),
        });
        let header = b.header;
        let result =
            b.as_mut()
                .try_set_layout_controllers(partial_modify_layout::LayoutControllers {
                    header: header + 2,
                });
        assert!(result.is_err());
    }

    #[define_varlen]
    pub struct GenericVarLenField<T: Default = usize> {
        #[varlen]
        arr: Array<T>,
    }

    #[define_varlen]
    pub struct GenericVarLenArrayField<T: Default = usize> {
        #[controls_layout]
        len: usize,
        #[varlen_array]
        arr: [T; *len],
    }

    #[define_varlen]
    pub struct NoGeneric {
        #[controls_layout]
        len: usize,
        #[varlen_array]
        arr: [u8; *len],
    }

    #[define_varlen]
    pub struct ReprRust {
        x: u8,
        y: u32,
        #[controls_layout]
        z: u8,
        #[varlen_array]
        arr: [u8; *z as usize],
    }

    #[define_varlen]
    #[repr(C)]
    pub struct ReprC {
        x: u8,
        y: u32,
        #[controls_layout]
        z: u8,
        #[varlen_array]
        arr: [u8; *z as usize],
    }

    #[test]
    fn test_repr() {
        assert!(std::mem::size_of::<ReprRust>() <= 8);
        assert_eq!(std::mem::size_of::<ReprC>(), 12);
        assert_eq!(0, offset_of!(ReprC, x));
        assert_eq!(4, offset_of!(ReprC, y));
        assert_eq!(8, offset_of!(ReprC, z));
    }

    
    #[cfg(not(miri))]
    #[test]
    fn compile_errors_are_good() {
        let t = trybuild::TestCases::new();
        t.compile_fail("shouldfail/*.rs");
    }
}
