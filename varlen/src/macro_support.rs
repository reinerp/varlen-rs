use core::mem::MaybeUninit;
use core::pin::Pin;
use core::ptr::NonNull;

use crate::array_init::ArrayInitializer;
use crate::{Initializer, Layout, VarLen};

// Array fields

#[inline(always)]
pub const fn cat_array_field_cautious<T>(
    array_len: usize,
    size: usize,
) -> Option<(
    /* offset */ usize,
    /* len */ usize,
    /* size afterwards */ usize,
)> {
    let elem_align = core::mem::align_of::<T>();
    let size = match size.checked_add(elem_align - 1) {
        Some(size) => size,
        None => return None,
    };
    let offset = size & (0usize.wrapping_sub(elem_align));
    let array_size = match core::mem::size_of::<T>().checked_mul(array_len) {
        Some(x) => x,
        None => return None,
    };
    let size = match offset.checked_add(array_size) {
        Some(x) => x,
        None => return None,
    };
    Some((offset, array_len, size))
}

#[inline(always)]
pub const fn cat_array_field_fast<T>(
    array_len: usize,
    size: usize,
) -> (
    /* offset */ usize,
    /* len */ usize,
    /* size afterwards */ usize,
) {
    let elem_align = core::mem::align_of::<T>();
    let size = size.wrapping_add(elem_align - 1);
    let offset = size & (0usize.wrapping_sub(elem_align));
    let array_size = core::mem::size_of::<T>().wrapping_mul(array_len);
    (offset, array_len, offset.wrapping_add(array_size))
}

#[inline(always)]
pub unsafe fn drop_array<T>(p: *mut u8, offset: usize, len: usize) {
    let slice = core::ptr::slice_from_raw_parts_mut(p.wrapping_add(offset) as *mut T, len);
    ::core::ptr::drop_in_place(slice);
}

#[inline(always)]
pub unsafe fn ref_array<T, U>(base: &T, offset: usize, len: usize) -> &[U] {
    core::slice::from_raw_parts(
        (base as *const T as *const u8).wrapping_add(offset) as *const U,
        len,
    )
}

#[inline(always)]
pub unsafe fn mut_array<'a, S, T>(base: *mut S, offset: usize, len: usize) -> &'a mut [T] {
    core::slice::from_raw_parts_mut((base as *mut u8).wrapping_add(offset) as *mut T, len)
}

pub unsafe fn init_array<T>(
    init: impl ArrayInitializer<T>,
    base: NonNull<u8>,
    offset: usize,
    len: usize,
) -> crate::marker::ArrayMarker<T> {
    let arr: &mut [MaybeUninit<T>] = core::slice::from_raw_parts_mut(
        base.as_ptr().wrapping_add(offset) as *mut MaybeUninit<T>,
        len,
    );
    init.initialize(arr);
    crate::marker::ArrayMarker::new_unchecked()
}

// Varlen fields

#[inline(always)]
pub fn cat_field_cautious<Field: VarLen, Init: Initializer<Field>>(
    init: &Init,
    offset: usize,
) -> Option<(
    /* offset */ usize,
    Field::Layout,
    /* size afterwards */ usize,
)> {
    let align = Field::ALIGN;
    let offset = match offset.checked_add(align - 1) {
        Some(o) => o,
        None => return None,
    };
    let offset = offset & 0usize.wrapping_sub(align);
    let layout = match init.calculate_layout_cautious() {
        Some(l) => l,
        None => return None,
    };
    let size = match offset.checked_add(layout.size()) {
        Some(s) => s,
        None => return None,
    };
    Some((offset, layout, size))
}

#[inline(always)]
pub fn cat_field_fast<Field: VarLen, Parent>(
    parent: &Parent,
    offset: usize,
) -> (
    /* offset */ usize,
    Field::Layout,
    /* size afterwards */ usize,
) {
    let align = Field::ALIGN;
    let offset = offset.wrapping_add(align - 1) & 0usize.wrapping_sub(align);

    let child: &Field =
        unsafe { &*((parent as *const Parent as *const u8).wrapping_add(offset) as *const Field) };
    let layout = child.calculate_layout();

    let size = offset.wrapping_add(layout.size());
    (offset, layout, size)
}

/// Safety: `p+offset` must have valid storage as specified by `layout`.
#[inline(always)]
pub unsafe fn init_field<Field: VarLen, Init: Initializer<Field>>(
    init: Init,
    p: NonNull<u8>,
    offset: usize,
    layout: Field::Layout,
) -> crate::marker::FieldMarker<Field> {
    let p = NonNull::new_unchecked(p.as_ptr().wrapping_add(offset) as *mut Field);
    init.initialize(p, layout);
    crate::marker::FieldMarker::new_unchecked()
}

/// Safety: `p+offset` must have a valid `Field` which matches `layout`.
#[inline(always)]
pub unsafe fn drop_field<Field: VarLen>(p: *mut u8, offset: usize, layout: Field::Layout) {
    let mut p = core::pin::Pin::new_unchecked(&mut *(p.wrapping_add(offset) as *mut Field));
    // Drop the field before its tail, because the field's destructor might reference its tail.
    core::ptr::drop_in_place(p.as_mut().get_unchecked_mut() as *mut Field);
    Field::drop_tail(p, layout);
}

#[inline(always)]
pub unsafe fn mut_field<'a, S, T>(mut_ptr: *mut S, offset: usize) -> Pin<&'a mut T> {
    Pin::new_unchecked(&mut *((mut_ptr as *mut u8).wrapping_add(offset) as *mut T))
}

#[inline(always)]
pub unsafe fn ref_field<S, T>(base: &S, offset: usize) -> &T {
    &*((base as *const S as *const u8).wrapping_add(offset) as *mut T)
}

// Other
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
