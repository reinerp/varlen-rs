use core::alloc as alloc;
use crate::{VarLen, VarLenInitializer, Layout};
use crate::owned::Owned;
use core::ptr::NonNull;
use core::pin::Pin;

pub struct Seq<T: VarLen> {
    // ptr's alignment is allegedly core::mem::align_of::<T>(), but in fact it's even
    // more than that: it's 
    ptr: NonNull<T>,
    len_logical: usize,
    occupied_bytes: usize,
    capacity_bytes: usize,
}

#[inline(never)]
#[cold]
fn layout_overflow() -> ! {
    panic!("Overflowing layout")
}

pub struct OverflowError;

#[inline(always)]
unsafe fn add_bytes_fast<T>(ptr: NonNull<T>, bytes: usize) -> NonNull<T> {
    NonNull::new_unchecked((ptr.as_ptr() as *mut u8).wrapping_add(bytes) as *mut T)
}

#[inline(always)]
fn round_up_fast(size: usize, align: usize) -> usize {
    size.wrapping_add(align - 1) & 0usize.wrapping_sub(align)
}

#[inline]
fn try_realloc(ptr: NonNull<u8>, capacity: usize, minimum: usize, align: usize) -> Result<(NonNull<u8>, usize), OverflowError> {
    let minimum = minimum.checked_add(align - 1).ok_or(OverflowError)?;
    let minimum = minimum & 0usize.wrapping_sub(align);
    let size = std::cmp::max(std::cmp::max(minimum, 8), capacity.checked_mul(2).ok_or(OverflowError)?);
    let old_layout = alloc::Layout::from_size_align(capacity, align).map_err(|_| OverflowError)?;
    let layout = alloc::Layout::from_size_align(size, align).map_err(|_| OverflowError)?;
    let ptr = if capacity == 0 {
        unsafe { std::alloc::alloc(layout) }
    } else {
        unsafe { std::alloc::realloc(ptr.as_ptr(), old_layout, size) }
    };
    let ptr = NonNull::new(ptr).ok_or(OverflowError)?;
    Ok((ptr, size))
}

#[inline(never)]
#[cold]
fn must_realloc(ptr: NonNull<u8>, capacity: usize, minimum: usize, align: usize) -> (NonNull<u8>, usize) {
    try_realloc(ptr, capacity, minimum, align).unwrap_or_else(|_| layout_overflow())
}

impl<T: VarLen> Seq<T> {
    /// Creates an empty sequence.
    #[inline]
    pub fn new() -> Self {
        Seq{
            ptr: NonNull::dangling(),
            len_logical: 0,
            occupied_bytes: 0,
            capacity_bytes: 0,
        }
    }

    /// Number of elements in the sequence.
    #[inline]
    pub fn len(&self) -> usize {
        self.len_logical
    }

    /// Adds an element to the sequence.
    #[inline]
    pub fn push(&mut self, init: impl VarLenInitializer<T>) {
        self.try_push(init).unwrap_or_else(|_| layout_overflow())
    }

    #[inline]
    fn try_push(&mut self, init: impl VarLenInitializer<T>) -> Result<(), OverflowError> {
        let layout = init.calculate_layout().ok_or(OverflowError)?;
        let occupied_plus = self.occupied_bytes.checked_add(layout.size()).ok_or(OverflowError)?;
        if occupied_plus > self.capacity_bytes {
            let (ptr, capacity) = must_realloc(self.ptr.cast(), self.capacity_bytes, occupied_plus, T::ALIGN);
            self.ptr = ptr.cast();
            self.capacity_bytes = capacity;
        }
        let occupied_plus = round_up_fast(occupied_plus, T::ALIGN);
        unsafe {
            init.initialize(add_bytes_fast(self.ptr, self.occupied_bytes), layout);
        }
        self.occupied_bytes = occupied_plus;
        self.len_logical += 1;
        Ok(())
    }

    /// Iterate over references.
    #[inline]
    pub fn iter(&self) -> Iter<'_, T> {
        Iter{
            ptr: self.ptr,
            len_logical: self.len_logical,
            marker: core::marker::PhantomData,
        }
    }

    /// Iterate over mutable (pinned) references.
    #[inline]
    pub fn iter_mut(&mut self) -> IterMut<'_, T> {
        IterMut{
            ptr: self.ptr,
            len_logical: self.len_logical,
            marker: core::marker::PhantomData,
        }
    }

    /// Iterate over `Owned<T>` values.
    /// 
    /// Ownership semantics is a little unusual:
    ///  * ownership (responsibility to `drop`) of the `T` values is transferred
    ///    to `OwnedElems`
    ///  * ownership (responsibility to `drop`) the storage remains with the `Seq`
    ///  * any access to the `Seq` after calling `take_elems` will see a logically
    ///    empty sequence, with large capacity.
    #[inline]
    pub fn take_elems(&mut self) -> OwnedElems<'_, T> {
        let r = OwnedElems{
            ptr: self.ptr,
            len_logical: self.len_logical,
            marker: core::marker::PhantomData,
        };
        self.len_logical = 0;
        self.occupied_bytes = 0;
        r
    }

    #[inline]
    fn layout(&self) -> alloc::Layout {
        unsafe {
            alloc::Layout::from_size_align_unchecked(self.capacity_bytes, T::ALIGN)
        }
    }
}

impl<T: VarLen> Drop for Seq<T> {
    fn drop(&mut self) {
        if self.capacity_bytes == 0 {
            return;
        }
        drop(self.take_elems());
        unsafe { std::alloc::dealloc(self.ptr.as_ptr() as *mut u8, self.layout()); }
    }
}

impl<T: VarLen, Init: VarLenInitializer<T>> Extend<Init> for Seq<T> {
    fn extend<I>(&mut self, iter: I) where I: IntoIterator<Item = Init> {
        for init in iter {
            self.push(init);
        }
    }
}

impl<T: VarLen, Init: VarLenInitializer<T>> FromIterator<Init> for Seq<T> {
    fn from_iter<I>(iter: I) -> Self where I: IntoIterator<Item = Init> {
        let mut seq = Seq::new();
        seq.extend(iter);
        seq
    }
}

/// Iterates over references.
pub struct Iter<'a, T>{
    ptr: NonNull<T>,
    len_logical: usize,
    marker: core::marker::PhantomData<&'a [T]>,
}

impl<'a, T: VarLen> Iter<'a, T> {
    /// Returns a copy of this iterator that is limited to at most `n` elements.
    /// 
    /// This is a slightly more efficient variant of `take`.
    #[inline]
    pub fn limited_to(&self, n: usize) -> Self {
        Iter {
            ptr: self.ptr,
            len_logical: core::cmp::min(self.len_logical, n),
            marker: core::marker::PhantomData,
        }
    }
}

impl<'a, T: VarLen> Iterator for Iter<'a, T> {
    type Item = &'a T;

    #[inline]
    fn next(&mut self) -> Option<&'a T> {
        if self.len_logical > 0 {
            let t = unsafe { self.ptr.as_ref() };
            let size = t.calculate_layout().size();
            self.ptr = unsafe { add_bytes_fast(self.ptr, round_up_fast(size, T::ALIGN)) };
            self.len_logical -= 1;
            Some(t)
        } else {
            None
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len_logical, Some(self.len_logical))
    }

    #[inline]
    fn count(self) -> usize {
        self.len_logical
    }
}

impl<'a, T: VarLen> ExactSizeIterator for Iter<'a, T> {
    #[inline]
    fn len(&self) -> usize {
        self.len_logical
    }
}

/// Iterates over mutable references.
pub struct IterMut<'a, T>{
    ptr: NonNull<T>,
    len_logical: usize,
    marker: core::marker::PhantomData<&'a mut [T]>,
}

impl<'a, T: VarLen> IterMut<'a, T> {
    /// Returns a copy of this iterator that is limited to at most `n` elements.
    /// 
    /// This is a slightly more efficient variant of `take`.
    #[inline]
    pub fn limited_to(mut self, n: usize) -> Self {
        self.len_logical = core::cmp::min(self.len_logical, n);
        self
    }
}

impl<'a, T: VarLen> Iterator for IterMut<'a, T> {
    type Item = Pin<&'a mut T>;

    #[inline]
    fn next(&mut self) -> Option<Pin<&'a mut T>> {
        if self.len_logical > 0 {
            let t = unsafe { Pin::new_unchecked(self.ptr.as_mut()) };
            let size = t.calculate_layout().size();
            self.ptr = unsafe { add_bytes_fast(self.ptr, round_up_fast(size, T::ALIGN)) };
            self.len_logical -= 1;
            Some(t)
        } else {
            None
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len_logical, Some(self.len_logical))
    }

    #[inline]
    fn count(self) -> usize {
        self.len_logical
    }
}

impl<'a, T: VarLen> ExactSizeIterator for IterMut<'a, T> {
    #[inline]
    fn len(&self) -> usize {
        self.len_logical
    }
}

/// Iterates over owned elements of a Seq. Walking this iterator takes ownership of the
/// elements, but ownership of the underlying byte storage remains with the Seq.
pub struct OwnedElems<'a, T: VarLen>{
    // Modified by iterator:
    ptr: NonNull<T>,
    len_logical: usize,
    marker: core::marker::PhantomData<&'a [u8]>,
}

impl<'a, T: VarLen> Iterator for OwnedElems<'a, T> {
    type Item = Owned<'a, T>;

    #[inline]
    fn next(&mut self) -> Option<Owned<'a, T>> {
        if self.len_logical > 0 {
            let t = unsafe { Owned::from_raw(self.ptr) };
            let size = T::calculate_layout(&*t).size();
            self.ptr = unsafe { add_bytes_fast(self.ptr, round_up_fast(size, T::ALIGN)) };
            self.len_logical -= 1;
            Some(t)
        } else {
            None
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len_logical, Some(self.len_logical))
    }

    #[inline]
    fn count(self) -> usize {
        self.len_logical
    }
}

impl<'a, T: VarLen> ExactSizeIterator for OwnedElems<'a, T> {
    #[inline]
    fn len(&self) -> usize {
        self.len_logical
    }
}

impl<'a, T: VarLen> Drop for OwnedElems<'a, T> {
    fn drop(&mut self) {
        if core::mem::needs_drop::<T>() || T::NEEDS_DROP_TAIL {
            while let Some(t) = self.next() {
                drop(t);
            }
        }
    }
}