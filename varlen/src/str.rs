use crate::array::{Array, ArrayLen};
use crate::varlen_newtype;
use core::pin::Pin;

varlen_newtype! {
    #[repr(transparent)]
    pub struct Str<(Len: ArrayLen = usize)>(Array<u8, Len>);

    with signature: impl<(Len: ArrayLen)> Str2<(Len)>;

    with init: pub struct StrInit<_>(pub _);
    with inner_ref: pub fn inner(&self) -> &_;
    with inner_mut: pub fn inner_mut(self: _) -> _;
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