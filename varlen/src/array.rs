use crate::marker::ArrayMarker;
use crate::{ArrayInitializer, Initializer, Layout, VarLen};
use core::pin::Pin;

pub struct Array<T, Len: ArrayLen = usize> {
    len: Len,
    _array: ArrayMarker<T>,
}

pub struct ArrayLayout {
    array_offset: usize,
    array_len: usize,
    size: usize,
}

impl Layout for ArrayLayout {
    #[inline(always)]
    fn size(&self) -> usize {
        self.size
    }
}

unsafe impl<T, Len: ArrayLen> VarLen for Array<T, Len> {
    type Layout = ArrayLayout;

    #[inline(always)]
    fn calculate_layout(&self) -> ArrayLayout {
        let offset = core::mem::size_of::<Self>();
        let (array_offset, array_len, size) = crate::macro_support::cat_array_field_fast::<T>(self.len.as_usize(), offset);
        ArrayLayout{
            array_offset,
            array_len,
            size,
        }
    }

    const ALIGN: usize = crate::macro_support::array_max(&[core::mem::align_of::<Self>(), core::mem::align_of::<T>()]);
    const NEEDS_DROP_TAIL: bool = core::mem::needs_drop::<T>();

    #[inline(always)]
    unsafe fn drop_tail(self: core::pin::Pin<&mut Self>, layout: Self::Layout) {
        crate::macro_support::drop_array::<T>(self.get_unchecked_mut() as *mut _ as *mut u8, layout.array_offset, layout.array_len);
    }
}

impl<T, Len: ArrayLen> core::ops::Deref for Array<T, Len> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        let layout = self.calculate_layout();
        unsafe {
            crate::macro_support::ref_array(self, layout.array_offset, layout.array_len)
        }
    }
}

impl<T, Len: ArrayLen> Array<T, Len> {
    pub fn mut_slice(self: Pin<&mut Self>) -> &mut [T] {
        let layout = self.calculate_layout();
        unsafe {
            crate::macro_support::mut_array(self.get_unchecked_mut() as *mut _, layout.array_offset, layout.array_len)
        }
    }
}

pub struct SizedInit<Init, Len: ArrayLen = usize>(pub Len, pub Init);

pub fn copy_from_slice<T: Copy, Len: ArrayLen>(src: &[T]) -> Option<SizedInit<crate::init::CopyFrom<T>, Len>> {
    let len = Len::from_usize(src.len())?;
    Some(SizedInit(
        len,
        crate::init::CopyFrom(src),
    ))
}   

pub fn clone_from_slice<T: Clone, Len: ArrayLen>(src: &[T]) -> Option<SizedInit<crate::init::CloneFrom<T>, Len>> {
    let len = Len::from_usize(src.len())?;
    Some(SizedInit(
        len,
        crate::init::CloneFrom(src),
    ))
}

unsafe impl<T, Len: ArrayLen, Init: ArrayInitializer<T>> Initializer<Array<T, Len>> for SizedInit<Init, Len> {
    #[inline(always)]
    fn calculate_layout_cautious(&self) -> Option<ArrayLayout> {
        let offset = core::mem::size_of::<Array<T, Len>>();
        let (array_offset, array_len, size) = crate::macro_support::cat_array_field_cautious::<T>(self.0.as_usize(), offset)?;
        Some(ArrayLayout{
            array_offset,
            array_len,
            size,
        })
    }

    unsafe fn initialize(self, dst: std::ptr::NonNull<Array<T, Len>>, layout: ArrayLayout) {
        let header = Array{
            len: self.0,
            _array: crate::macro_support::init_array(self.1, dst.cast::<u8>(), layout.array_offset, layout.array_len),
        };
        core::ptr::write(dst.as_ptr(), header);
    }
}

pub trait ArrayLen: Copy + private::Sealed {
    fn as_usize(self) -> usize;
    fn from_usize(x: usize) -> Option<Self>;
}

impl ArrayLen for usize {
    #[inline(always)]
    fn as_usize(self) -> usize {
        self
    }

    #[inline(always)]
    fn from_usize(x: usize) -> Option<Self> {
        Some(x)
    }
}

mod private {
    pub trait Sealed {}

    impl Sealed for usize {}
}
