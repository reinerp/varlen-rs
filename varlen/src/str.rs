use crate::array::{Array, ArrayLen};
use crate::{varlen_newtype, Initializer};
use core::pin::Pin;

varlen_newtype! {
    #[repr(transparent)]
    /// Inline utf-8 string; the variable-length analog of `String`.
    /// 
    /// You may select the length field from among u8/u16/u32/u64/usize.
    /// The choice of the length field will limit what the longest representable
    /// string is.
    pub struct Str<(Len: ArrayLen = usize)>(Array<u8, Len>);

    with signature: impl<(Len: ArrayLen)> Str<(Len)>;

    with init: struct StrInit<_>(_);
    with inner_ref: fn inner(&self) -> &_;
    with inner_mut: fn inner_mut(self: _) -> _;
}

impl<Len: ArrayLen> core::ops::Deref for Str<Len> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        let slice: &[u8] = &self.inner();
        unsafe {
            // Safety: we only allow initialization from valid utf8
            core::str::from_utf8_unchecked(slice)
        }
    }
}

impl<Len: ArrayLen> Str<Len> {
    pub fn as_mut(self: Pin<&mut Self>) -> &mut str {
        let slice = self.inner_mut().mut_slice();
        unsafe {
            // Safety: we only allow initialization from valid utf8
            core::str::from_utf8_unchecked_mut(slice)
        }
    }
}

impl<Len: ArrayLen> Str<Len> {
    pub fn copy_from_str(s: &str) -> Option<impl Initializer<Self> + '_> {
        Some(StrInit(Array::copy_from_slice(s.as_bytes())?))
    }
}