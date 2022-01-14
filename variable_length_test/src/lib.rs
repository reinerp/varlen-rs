#[cfg(test)]
mod tests {
    use variable_length::define_varlen;
    use variable_length::boxed::Box;
    use variable_length::init::FillSequentially;

    #[define_varlen]
    struct T {
        inline_len: usize,

        #[varlen]
        arr: [u8; self.inline_len],

        #[varlen]
        arr2: [u16; self.inline_len * 4],
    }

    #[test]
    fn field_access_and_modify() {
        let mut b = Box::<T>::new(t::Init{
            header: t::Header {
                inline_len: 4,
            },
            arr: FillSequentially(|i| i as u8),
            arr2: FillSequentially(|i| i as u16),
        });
        assert_eq!(b.header().inline_len, 4);
        assert_eq!(b.arr(), &[0, 1, 2, 3]);
        b.as_mut().arr_mut()[2] = 4;
        assert_eq!(b.arr(), &[0, 1, 4, 3]);
        assert_eq!(b.arr2(), &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]);
        b.as_mut().arr2_mut()[2] = 4;
        assert_eq!(b.arr2(), &[0, 1, 4, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]);
    }
}
