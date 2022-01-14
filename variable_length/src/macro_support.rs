use core::alloc::Layout;
use core::ptr::NonNull;
use core::pin::Pin;

#[inline(always)]
pub const fn round_up(size: usize, align: usize) -> Option<usize> {
    let size_up = match size.checked_add(align - 1) {
        Some(size_up) => size_up,
        None => return None,
    };
    Some(size_up & (0usize.wrapping_sub(align)))
}

#[inline(always)]
pub const fn max(x: usize, y: usize) -> usize {
    if x > y { x } else { y }
}

pub struct UncheckedLayout {
    pub size: usize,
    pub align: usize,
}

#[inline(always)]
pub const fn to_alloc_layout(x: Option<UncheckedLayout>) -> Option<Layout> {
    let x = match x {
        Some(x) => x,
        None => return None, 
    };
    match Layout::from_size_align(x.size, x.align) {
        Ok(l) => Some(l),
        Err(_) => None,
    }
}

#[inline(always)]
pub const fn layout_of<T>() -> Option<UncheckedLayout> {
    Some(raw_layout_of::<T>())
}

#[inline(always)]
pub const fn raw_layout_of<T>() -> UncheckedLayout {
    UncheckedLayout{
        size: core::mem::size_of::<T>(),
        align: core::mem::align_of::<T>(),
    }
}

#[inline(always)]
pub const fn cat_array_cautious<T>(size: Option<usize>, array_len: usize) -> Option<usize> {
    let size = match size {
        Some(size) => size,
        None => return None,
    };
    let elem_align = core::mem::align_of::<T>();
    let size = match size.checked_add(elem_align - 1) {
        Some(size) => size,
        None => return None,
    };
    let size = size & (0usize.wrapping_sub(elem_align));
    let array_size = match core::mem::size_of::<T>().checked_mul(array_len) {
        Some(x) => x,
        None => return None,
    };
    size.checked_add(array_size)
}

#[inline(always)]
pub const fn size_to_layout_cautious(size: Option<usize>, align: usize) -> Option<Layout> {
    let size = match size {
        Some(size) => size,
        None => return None,
    };
    match Layout::from_size_align(size, align) {
        Ok(l) => Some(l),
        Err(_) => None,
    }
}

#[inline(always)]
pub const fn round_array_fast<T>(size: usize) -> usize {
    let elem_align = core::mem::align_of::<T>();
    size.wrapping_add(elem_align - 1) & (0usize.wrapping_sub(elem_align))
}

#[inline(always)]
pub const fn add_array_fast<T>(size: usize, array_len: usize) -> usize {
    let elem_size = core::mem::size_of::<T>();
    size.wrapping_add(elem_size.wrapping_mul(array_len))
}

#[inline(always)]
pub const fn cat_array_fast<T>(size: usize, array_len: usize) -> usize {
    let elem_align = core::mem::align_of::<T>();
    let size = size.wrapping_add(elem_align - 1);
    let size = size & (0usize.wrapping_sub(elem_align));
    let array_size = core::mem::size_of::<T>().wrapping_mul(array_len);
    size.wrapping_add(array_size)
}

#[inline(always)]
pub const unsafe fn to_alloc_layout_unchecked(x: UncheckedLayout) -> Layout {
    Layout::from_size_align_unchecked(x.size, x.align)
}

#[inline(always)]
pub unsafe fn slice_ptr<T>(base: NonNull<u8>, offset: usize, len: usize) -> NonNull<[T]> {
    NonNull::new_unchecked(core::ptr::slice_from_raw_parts_mut(base.as_ptr().wrapping_add(offset) as *mut T, len))
}

#[inline(always)]
pub unsafe fn slice_ref<T, U>(base: &T, offset: usize, len: usize) -> &[U] {
    core::slice::from_raw_parts((base as *const T as *const u8).wrapping_add(offset) as *const U, len)
}

#[inline(always)]
pub unsafe fn slice_mut_ref<T, U>(base: Pin<&mut T>, offset: usize, len: usize) -> &mut [U] {
    core::slice::from_raw_parts_mut((base.get_unchecked_mut() as *mut T as *mut u8).wrapping_add(offset) as *mut U, len)
}

#[inline(always)]
pub unsafe fn slice_mut_ref_split<'a, T, U>(base: Pin<&mut T>, offset: usize, len: usize) -> &'a mut [U] {
    core::slice::from_raw_parts_mut((base.get_unchecked_mut() as *mut T as *mut u8).wrapping_add(offset) as *mut U, len)
}

#[inline(always)]
pub const fn array_max(arr: &[usize]) -> usize {
    let mut i = 0;
    let mut r = usize::MIN;
    while i < arr.len() {
        let v = arr[i];
        r = if r > v { r } else { v };
        i += 1;
    }
    r
}