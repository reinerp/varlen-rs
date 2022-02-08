#![doc = svgbobdoc::transform!(
    //! A sequence of variable-length objects in a flat buffer.
    //! 
    //! For example, the [`Seq<Str<u8>>`] representation of `["hello", "good", "world!"]` is:
    //! 
    //! ```svgbob
    //! "Seq  (actual layout may vary)"
    //! +------+--------+--------------+--------------+
    //! | base | len: 3 | occupied_end | capacity_end |
    //! +------+--------+--------------+--------------+
    //!    |                   |              |
    //!    |                   |              '----------------------.
    //!    '-.                 '-----------------.                   |
    //!      |                                   |                   |
    //!      v  "Storage"                        v                   v
    //!      +---+-------+---+------+---+--------+-------------------+
    //!      | 5 | hello | 4 | good | 6 | world! | "<uninitialized>" |
    //!      +---+-------+---+------+---+--------+-------------------+
    //! ```
    //! 
    //! Unlike [`Vec<T>`], once an element has been pushed onto the sequence, its size cannot change. Also
    //! unlike [`Vec<T>`] there is no random access to the `n`th element of the sequence; access is primarily
    //! sequential although some indexing options exist. (TODO: explain)
    //! 
    //! # Examples
    //! 
    //! Like [`Vec<T>`], this type is a sequence of objects supporting amortized-O(1) [`push()`](`Seq::push`), and
    //! you can iterate through elements of the sequence:
    //! 
    //! ```
    //! # use varlen::{Seq, Str};
    //! let mut seq: Seq<Str> = Seq::new();
    //! seq.push(Str::copy_from_str("hello"));
    //! seq.push(Str::copy_from_str("good"));
    //! seq.push(Str::copy_from_str("world!"));
    //! let v: Vec<&str> = seq.iter().map(|s| &s[..]).collect();
    //! assert_eq!(vec!["hello", "good", "world!"], v);
    //! ```
    //! 
    //! # Module contents
    //! 
    //! The main type is [`Seq<T>`]. It includes iterator types [`Iter<'a, T>`], [`IterMut<'a, T>`] and
    //! [`OwnedElems<'a, T>`]. 
    //! 
    //! Types [`CheckedIndexing`] and [`UncheckedIndexing`] parametrize the [`Seq<T>`] and control whether
    //! random accessing is available valid length types for the array are [`ArrayLen`], its memory layout is
    //! calculated and stored in [`ArrayLayout`], and its general initializer is [`SizedInit`].
    )]
    

use crate::owned::Owned;
use crate::{Initializer, Layout, VarLen};
use core::alloc;
use core::pin::Pin;
use core::ptr::NonNull;

/// Controls what kind of indexing is available on a `Seq`: either `CheckedIndexing` or `UncheckedIndexing`.
pub trait Indexing: private::Sealed {}

mod private {
    use super::*;

    pub trait Sealed {
        const INSTANCE: Self;
        unsafe fn alloc_storage(layout: alloc::Layout) -> Option<NonNull<u8>>;
        unsafe fn grow_storage(
            ptr: *mut u8,
            layout: alloc::Layout,
            new_size: usize,
        ) -> Option<NonNull<u8>>;
        unsafe fn mark_offset_valid<T: VarLen>(
            ptr: NonNull<T>,
            capacity_offsets: usize,
            offset: usize,
        );
        unsafe fn clear_bitmasks<T: VarLen>(
            ptr: NonNull<T>,
            capacity_offsets: usize,
            occupied_offsets: usize,
        );
        fn extended_layout(layout: alloc::Layout) -> alloc::Layout;
    }
}

/// Unchecked indexing strategy for `Seq`. In this mode, the `Seq` includes only the storage
/// for the `T` objects, and nothing else. It is not possible to check integer offsets for
/// validity in this mode, so only unsafe unchecked indexing is available.
pub struct UncheckedIndexing;

impl private::Sealed for UncheckedIndexing {
    const INSTANCE: Self = UncheckedIndexing;
    unsafe fn alloc_storage(layout: alloc::Layout) -> Option<NonNull<u8>> {
        NonNull::new(std::alloc::alloc(layout))
    }
    unsafe fn grow_storage(
        ptr: *mut u8,
        layout: alloc::Layout,
        new_size: usize,
    ) -> Option<NonNull<u8>> {
        NonNull::new(std::alloc::realloc(ptr, layout, new_size))
    }

    #[inline(always)]
    unsafe fn mark_offset_valid<T: VarLen>(
        _ptr: NonNull<T>,
        _capacity_offsets: usize,
        _offset: usize,
    ) {
    }

    #[inline(always)]
    unsafe fn clear_bitmasks<T: VarLen>(
        _ptr: NonNull<T>,
        _capacity_offsets: usize,
        _occupied_offsets: usize,
    ) {
    }

    fn extended_layout(layout: alloc::Layout) -> alloc::Layout {
        layout
    }
}
impl Indexing for UncheckedIndexing {}

/// Checked indexing strategy for `Seq`. In this mode, the `Seq` includes an additional bitmask
/// which indicates the locations at which `T` objects start. Appending to the `Seq` updates the
/// bitmask. In this mode, safe (checked) indexing is available, which will either return to a
/// valid object or will fail at runtime (with either `None` or a panic).
/// 
/// This mode has some performance overheads relative to `UncheckedIndexing` mode:
/// 
/// * We consume additional memory: 1 bit overhead per `T::ALIGN` bytes of storage. The max
///   is when `T::ALIGN=1`, in which case there is 12.5% of memory overhead.
/// * Modifying (appending or clearing) the sequence has some runtime overhead, as it must
///   update the bitmask in addition to the underlying byte storage.
/// 
/// You should only enable this mode if you want to use safe indexing by integers.
pub struct CheckedIndexing;

#[inline]
fn add_bitmask_to_layout(layout: alloc::Layout) -> Option<(alloc::Layout, usize)> {
    assert!(layout.size() % layout.align() == 0);
    let offsets = layout.size() / layout.align();
    let bitmask_bytes = offsets.checked_add(7)? / 8;
    let full_layout =
        alloc::Layout::from_size_align(layout.size() + bitmask_bytes, layout.align()).ok()?;
    Some((full_layout, bitmask_bytes))
}

impl private::Sealed for CheckedIndexing {
    const INSTANCE: Self = CheckedIndexing;
    unsafe fn alloc_storage(layout: alloc::Layout) -> Option<NonNull<u8>> {
        let (full_layout, bitmask_bytes) = add_bitmask_to_layout(layout)?;
        let ptr = NonNull::new(std::alloc::alloc(full_layout))?;
        // Zero the bitmask.
        let bitmask =
            core::slice::from_raw_parts_mut(ptr.as_ptr().add(layout.size()), bitmask_bytes);
        bitmask.fill(0);
        Some(ptr)
    }

    unsafe fn grow_storage(
        ptr: *mut u8,
        old_layout: alloc::Layout,
        new_size: usize,
    ) -> Option<NonNull<u8>> {
        debug_assert!(new_size >= old_layout.size());
        let (old_full_layout, old_bitmask_bytes) = add_bitmask_to_layout(old_layout)?;
        let (new_full_layout, new_bitmask_bytes) = add_bitmask_to_layout(
            alloc::Layout::from_size_align(new_size, old_layout.align()).ok()?,
        )?;
        let ptr = NonNull::new(std::alloc::realloc(
            ptr,
            old_full_layout,
            new_full_layout.size(),
        ))?;
        let old_bitmask =
            core::slice::from_raw_parts(ptr.as_ptr().add(old_layout.size()), old_bitmask_bytes);
        let new_bitmask =
            core::slice::from_raw_parts_mut(ptr.as_ptr().add(new_size), new_bitmask_bytes);
        debug_assert!(new_bitmask_bytes >= old_bitmask_bytes);
        core::ptr::copy(
            old_bitmask.as_ptr(),
            new_bitmask.as_mut_ptr(),
            old_bitmask_bytes,
        );
        new_bitmask[old_bitmask_bytes..].fill(0);
        Some(ptr)
    }

    #[inline(always)]
    unsafe fn mark_offset_valid<T: VarLen>(
        ptr: NonNull<T>,
        capacity_offsets: usize,
        offset: usize,
    ) {
        let ptr = ptr.cast::<u8>().as_ptr();
        *ptr.wrapping_add(capacity_offsets * T::ALIGN + (offset / 8)) |= 1u8 << (offset % 8);
    }

    #[inline]
    unsafe fn clear_bitmasks<T: VarLen>(
        ptr: NonNull<T>,
        capacity_offsets: usize,
        occupied_offsets: usize,
    ) {
        let num_bytes = (occupied_offsets + 7) / 8;
        let bitmask_start = ptr.as_ptr().cast::<u8>().add(capacity_offsets * T::ALIGN);
        core::slice::from_raw_parts_mut(bitmask_start, num_bytes).fill(0);
    }

    #[inline]
    fn extended_layout(layout: alloc::Layout) -> alloc::Layout {
        add_bitmask_to_layout(layout)
            .unwrap_or_else(|| layout_overflow())
            .0
    }
}
impl Indexing for CheckedIndexing {}

#[doc = svgbobdoc::transform!(
/// A sequence of variable-length objects in a flat buffer.
/// 
/// For example, the [`Seq<Str<u8>>`] representation of `["hello", "good", "world!"]` is:
/// 
/// ```svgbob
/// "Seq (actual layout may vary)"
/// +------+--------+--------------+--------------+
/// | base | len: 3 | occupied_end | capacity_end |
/// +------+--------+--------------+--------------+
///    |                   |              |
///    |                   |              '----------------------.
///    '-.                 '-----------------.                   |
///      |                                   |                   |
///      v  "Storage"                        v                   v
///      +---+-------+---+------+---+--------+-------------------+
///      | 5 | hello | 4 | good | 6 | world! | "<uninitialized>" |
///      +---+-------+---+------+---+--------+-------------------+
/// ```
/// 
/// # Examples
/// 
/// Like [`Vec<T>`], this type is a sequence of objects supporting amortized-O(1) [`push()`](`Seq::push`), and
/// you can iterate through elements of the sequence:
/// 
/// ```
/// # use varlen::{Seq, Str};
/// let mut seq: Seq<Str> = Seq::new();
/// seq.push(Str::copy_from_str("hello"));
/// seq.push(Str::copy_from_str("good"));
/// seq.push(Str::copy_from_str("world!"));
/// let v: Vec<&str> = seq.iter().map(|s| &s[..]).collect();
/// assert_eq!(vec!["hello", "good", "world!"], v);
/// ```
/// 
/// *Unlike* [`Vec<T>`], there is no random access to the `n`th element of the sequence, 
/// Unlike [`Vec<T>`], once an element has been pushed onto the sequence, its size cannot change.
/// 
/// # Indexing
/// 
/// Since the `T` elements are variable-sized, the address of the `n`th element in the storage 
/// cannot directly be calculated from `n`. This means that random access to the `n`th element is 
/// not available in the [`Seq<T>`] type. However, some alternatives exist, either in variations 
/// of the [`Seq<T>`] type or in different kinds of indexing.
/// 
/// ## Indexing by offset
/// Instead of indexing by element index, you may index by *offset*. The 
/// offset of an element is defined to be its position in the storage buffer, in units of 
/// `T::ALIGN`. Offsets start at 0 and increase as you push elements onto a sequence, but they may
/// skip values:
/// 
/// ```
/// # use varlen::{Seq, Str};
/// let mut seq: Seq<Str<u16>> = Seq::new();
/// assert_eq!(0, seq.offset());
/// seq.push(Str::try_copy_from_str("hello").unwrap());
/// // "hello" takes u16 length, plus 5 bytes storage. Total: 7 bytes, which is 
/// // `div_round_up(7, Str::<u16>::ALIGN)=4` offsets.
/// assert_eq!(4, seq.offset());
/// seq.push(Str::try_copy_from_str("fantastic").unwrap());
/// assert_eq!(10, seq.offset());
/// seq.push(Str::try_copy_from_str("world").unwrap());
/// assert_eq!(14, seq.offset());
/// ```
/// 
/// In [`Seq<T>`] indexing by offset is available, but unsafe, because the data structure doesn't
/// contain enough information to validate an offset:
/// 
/// ```
/// // ... continued
/// # use varlen::{Seq, Str};
/// # let mut seq: Seq<Str<u16>> = Seq::new();
/// # assert_eq!(0, seq.offset());
/// # seq.push(Str::try_copy_from_str("hello").unwrap());
/// # // "hello" takes u16 length, plus 5 bytes storage. Total: 7 bytes, which is 
/// # // `div_round_up(7, Str::<u16>::ALIGN)=4` offsets.
/// # assert_eq!(4, seq.offset());
/// # seq.push(Str::try_copy_from_str("fantastic").unwrap());
/// # assert_eq!(10, seq.offset());
/// # seq.push(Str::try_copy_from_str("world").unwrap());
/// # assert_eq!(14, seq.offset());
/// let s = unsafe { seq.from_offset_unchecked(4) };
/// assert_eq!("fantastic", &s[..]);
/// ```
/// 
/// In [`Seq<T, CheckedIndexing>`] indexing by offset is available and safe:
/// 
/// ```
/// # use varlen::{Seq, Str};
/// # use varlen::seq::CheckedIndexing;
/// let mut seq: Seq<Str<u16>, CheckedIndexing> = Seq::new();
/// seq.push(Str::try_copy_from_str("hello").unwrap());
/// assert_eq!(4, seq.offset());
/// seq.push(Str::try_copy_from_str("fantastic").unwrap());
/// seq.push(Str::try_copy_from_str("world").unwrap());
/// assert_eq!("fantastic", &seq.from_offset(4)[..]);
/// // Would panic: seq.from_offset(5)
/// ```
/// 
/// To make this possible, [`Seq<T, CheckedIndexing>`] stores a bitmap indicating where the start
/// of every element in the storage is:
///
/// ```svgbob
/// "Seq (actual layout may vary)"
/// +------+--------+--------------+--------------+
/// | base | len: 3 | occupied_end | capacity_end |
/// +------+--------+--------------+--------------+
///    |                   |              |
///    |                   |              '----------------------.
///    '-.                 '-----------------.                   |
///      |                                   |                   |
///      v  "Storage"                        v                   v
///      +---+-------+---+-------+---+-------+-------------------+
///      | 5 | hello | 4 | good  | 6 | world | "<uninitialized>" |
///      +---+-------+---+-------+---+-------+-------------------+
///      ^           ^           ^       
///      |           |           |
///      v           v           v
///      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-------------------+
///      |1|0|0|0|0|0|1|0|0|0|0|0|1|0|0|0|0|0| "<uninitialized>" |
///      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-------------------+
///      "Bitmap"
/// ```
/// 
/// Maintaining this bitmap adds a memory overhead of 1 bit per `T::ALIGN` bytes (at most 12.5% 
/// overhead in the case `T::ALIGN=1`), and slightly increases the cost of [`Seq::push`].

)]
pub struct Seq<T: VarLen, Idx: Indexing = UncheckedIndexing> {
    // Actually aligned to T::ALIGN, which is at least as large as core::mem::align_of::<T>().
    //
    // If B=UseBitmask, additionally has `capacity_bytes / (8 * T::ALIGN)` bytes of storage
    // immediately afterwards (at offset `capacity_bytes`), with set bits indicating the start
    // of an element.
    ptr: NonNull<T>,
    len_elements: usize,     // Number of elements
    occupied_offsets: usize, // Units of T::ALIGN
    capacity_offsets: usize, // Units of T::ALIGN
    _idx: Idx,
}

/// A `Seq` with safe indexing by integers.
pub type IndexableSeq<T> = Seq<T, CheckedIndexing>;

#[inline(never)]
#[cold]
fn layout_overflow() -> ! {
    panic!("Overflowing layout")
}

#[inline(never)]
#[cold]
fn out_of_bounds() -> ! {
    panic!("Out-of-bounds access")
}

#[inline(never)]
#[cold]
fn invalid_offset() -> ! {
    panic!("Invalid Seq offset")
}

pub struct OverflowError;

#[inline(always)]
unsafe fn add_offsets_fast<T: VarLen>(ptr: NonNull<T>, offsets: usize) -> NonNull<T> {
    add_bytes_fast(ptr, offsets * T::ALIGN)
}

#[inline(always)]
unsafe fn add_bytes_fast<T: VarLen>(ptr: NonNull<T>, bytes: usize) -> NonNull<T> {
    NonNull::new_unchecked((ptr.as_ptr() as *mut u8).wrapping_add(bytes) as *mut T)
}

#[inline(always)]
fn round_up_fast(size: usize, align: usize) -> usize {
    size.wrapping_add(align - 1) & 0usize.wrapping_sub(align)
}

#[inline]
fn try_realloc<Idx: Indexing>(
    ptr: NonNull<u8>,
    capacity_offsets: usize,
    minimum_offsets: usize,
    align: usize,
) -> Result<(NonNull<u8>, usize), OverflowError> {
    let size_offsets = std::cmp::max(
        std::cmp::max(minimum_offsets, (32 + align - 1) /  align),
        capacity_offsets.checked_mul(2).ok_or(OverflowError)?,
    );
    let size_bytes = size_offsets.checked_mul(align).ok_or(OverflowError)?;
    let old_layout = alloc::Layout::from_size_align(capacity_offsets * align, align)
        .map_err(|_| OverflowError)?;
    let layout = alloc::Layout::from_size_align(size_bytes, align).map_err(|_| OverflowError)?;
    let ptr = if capacity_offsets == 0 {
        unsafe { Idx::alloc_storage(layout) }
    } else {
        unsafe { Idx::grow_storage(ptr.as_ptr(), old_layout, size_bytes) }
    };
    let ptr = ptr.ok_or(OverflowError)?;
    Ok((ptr, size_offsets))
}

#[inline(never)]
#[cold]
fn must_realloc<Idx: Indexing>(
    ptr: NonNull<u8>,
    capacity_offsets: usize,
    minimum_offsets: usize,
    align: usize,
) -> (NonNull<u8>, usize) {
    try_realloc::<Idx>(ptr, capacity_offsets, minimum_offsets, align)
        .unwrap_or_else(|_| layout_overflow())
}

impl<T: VarLen> Seq<T> {
    #[inline]
    pub fn new_sequential() -> Self {
        Self::new()
    }
}

impl<T: VarLen> Seq<T, CheckedIndexing> {
    #[inline]
    pub fn new_indexable() -> Self {
        Self::new()
    }
}

impl<T: VarLen, Idx: Indexing> Seq<T, Idx> {
    /// Creates an empty sequence.
    #[inline]
    pub fn new() -> Self {
        Seq {
            ptr: NonNull::dangling(),
            len_elements: 0,
            occupied_offsets: 0,
            capacity_offsets: 0,
            _idx: Idx::INSTANCE,
        }
    }

    /// Number of elements in the sequence.
    #[inline]
    pub fn len(&self) -> usize {
        self.len_elements
    }

    /// Number of offsets (units of `T::ALIGN`) in the storage.
    #[inline]
    pub fn capacity_in_offsets(&self) -> usize {
        self.capacity_offsets
    }

    /// Offset of the next element to be added to the sequence.
    #[inline]
    pub fn offset(&self) -> usize {
        self.occupied_offsets
    }

    /// Adds an element to the sequence.
    #[inline]
    pub fn push(&mut self, init: impl Initializer<T>) {
        self.try_push(init).unwrap_or_else(|_| layout_overflow())
    }

    #[inline]
    fn try_push(&mut self, init: impl Initializer<T>) -> Result<(), OverflowError> {
        let layout = init.calculate_layout_cautious().ok_or(OverflowError)?;
        let align = T::ALIGN;
        let size_offsets = layout.size().checked_add(align - 1).ok_or(OverflowError)? / align;
        let occupied_plus = self
            .occupied_offsets
            .checked_add(size_offsets)
            .ok_or(OverflowError)?;
        if occupied_plus > self.capacity_offsets {
            let (ptr, capacity) = must_realloc::<Idx>(
                self.ptr.cast(),
                self.capacity_offsets,
                occupied_plus,
                T::ALIGN,
            );
            self.ptr = ptr.cast();
            self.capacity_offsets = capacity;
        }
        let elem_offset = self.occupied_offsets;
        unsafe {
            Idx::mark_offset_valid(self.ptr, self.capacity_offsets, elem_offset);
            init.initialize(add_offsets_fast(self.ptr, elem_offset), layout);
        }
        self.occupied_offsets = occupied_plus;
        self.len_elements += 1;
        Ok(())
    }

    /// Iterate over references.
    #[inline]
    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            ptr: self.ptr,
            len_elements: self.len_elements,
            marker: core::marker::PhantomData,
        }
    }

    /// Iterate over mutable (pinned) references.
    #[inline]
    pub fn iter_mut(&mut self) -> IterMut<'_, T> {
        IterMut {
            ptr: self.ptr,
            len_elements: self.len_elements,
            marker: core::marker::PhantomData,
        }
    }

    /// Gets the offset of this element from the start of the contiguous storage, counted in
    /// multiples of `<T as VarLen>::ALIGN`.
    ///
    /// Panics if this element isn't from this `Seq`'s storage.
    #[inline]
    pub fn offset_of(&self, element: &T) -> usize {
        let byte_offset = (element as *const T as usize).wrapping_sub(self.ptr.as_ptr() as usize);
        if byte_offset >= self.occupied_offsets {
            out_of_bounds()
        } else {
            debug_assert!(byte_offset % T::ALIGN == 0);
            byte_offset / T::ALIGN
        }
    }

    /// Given an offset of an element from the start of the continuous storage (counted in multiples
    /// of `<T as VarLen>::ALIGN`), returns a reference to the element.
    ///
    /// Safety
    /// * must be a valid offset to an element in this sequence. The canonical way to find this
    ///   is by `offset_of()` on an element from this sequence. Such offsets remain valid for all
    ///   immutable operations on the sequence, and also for `push()` on the sequence. The offset
    ///   is invalidated by any mutable operation that removes the specified element, or any earlier
    ///   element, from the sequence.
    /// 
    /// For a safe variant, see [`Self::from_offset`].
    #[inline]
    pub unsafe fn from_offset_unchecked(&self, offset: usize) -> &T {
        debug_assert!(offset < self.occupied_offsets);
        &*self
            .ptr
            .as_ptr()
            .cast::<u8>()
            .wrapping_add(offset * T::ALIGN)
            .cast::<T>()
    }

    /// Given an offset of an element from the start of the continuous storage (counted in multiples
    /// of `<T as VarLen>::ALIGN`), returns a reference to the element.
    ///
    /// Safety
    /// * must be a valid offset to an element in this sequence. The canonical way to find this
    ///   is by `offset_of()` on an element from this sequence. Such offsets remain valid for all
    ///   immutable operations on the sequence, and also for `push()` on the sequence. The offset
    ///   is invalidated by any mutable operation that removes the specified element, or any earlier
    ///   element, from the sequence.
    /// 
    /// For a safe variant, see [`Self::from_offset_mut`].
    #[inline]
    pub unsafe fn from_offset_unchecked_mut(&mut self, offset: usize) -> Pin<&mut T> {
        debug_assert!(offset < self.occupied_offsets);
        Pin::new_unchecked(
            &mut *self
                .ptr
                .as_ptr()
                .cast::<u8>()
                .wrapping_add(offset * T::ALIGN)
                .cast::<T>(),
        )
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
        let r = OwnedElems {
            ptr: self.ptr,
            len_elements: self.len_elements,
            marker: core::marker::PhantomData,
        };
        self.len_elements = 0;
        self.occupied_offsets = 0;
        unsafe {
            Idx::clear_bitmasks(self.ptr, self.capacity_offsets, self.occupied_offsets);
        }
        r
    }

    #[inline]
    fn layout(&self) -> alloc::Layout {
        let base_layout = unsafe {
            alloc::Layout::from_size_align_unchecked(self.capacity_offsets * T::ALIGN, T::ALIGN)
        };
        Idx::extended_layout(base_layout)
    }
}

impl<T: VarLen> Seq<T, CheckedIndexing> {
    /// Gets a reference to the element at the specified offset (measured in units of `T::ALIGN`).
    #[inline]
    pub fn from_offset(&self, offset: usize) -> &T {
        if self.is_offset_valid(offset) {
            // Safety: we just checked offset validity!
            unsafe { self.from_offset_unchecked(offset) }
        } else {
            invalid_offset()
        }
    }

    /// Gets a mutable reference to the element at the specified offset (measured in units of `T::ALIGN`).
    #[inline]
    pub fn from_offset_mut(&mut self, offset: usize) -> Pin<&mut T> {
        if self.is_offset_valid(offset) {
            // Safety: we just checked offset validity!
            unsafe { self.from_offset_unchecked_mut(offset) }
        } else {
            invalid_offset()
        }
    }

    /// Checks whether the specified offset is valid for this sequence.
    #[inline]
    pub fn is_offset_valid(&self, offset: usize) -> bool {
        if offset >= self.occupied_offsets {
            return false;
        }
        let ptr = self.ptr.cast::<u8>().as_ptr() as *const u8;
        unsafe {
            *ptr.wrapping_add(self.capacity_offsets * T::ALIGN + (offset / 8))
                & (1u8 << (offset % 8))
                != 0
        }
    }
}

impl<T: VarLen, Idx: Indexing> Drop for Seq<T, Idx> {
    fn drop(&mut self) {
        if self.capacity_offsets == 0 {
            return;
        }
        drop(self.take_elems());
        unsafe {
            std::alloc::dealloc(self.ptr.as_ptr() as *mut u8, self.layout());
        }
    }
}

impl<T: VarLen, Init: Initializer<T>, Idx: Indexing> Extend<Init> for Seq<T, Idx> {
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = Init>,
    {
        for init in iter {
            self.push(init);
        }
    }
}

impl<T: VarLen, Init: Initializer<T>, Idx: Indexing> FromIterator<Init> for Seq<T, Idx> {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = Init>,
    {
        let mut seq = Seq::new();
        seq.extend(iter);
        seq
    }
}

/// Iterates over references.
pub struct Iter<'a, T> {
    ptr: NonNull<T>,
    len_elements: usize,
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
            len_elements: core::cmp::min(self.len_elements, n),
            marker: core::marker::PhantomData,
        }
    }
}

impl<'a, T: VarLen> Iterator for Iter<'a, T> {
    type Item = &'a T;

    #[inline]
    fn next(&mut self) -> Option<&'a T> {
        if self.len_elements > 0 {
            let t = unsafe { self.ptr.as_ref() };
            let size = t.calculate_layout().size();
            self.ptr = unsafe { add_bytes_fast(self.ptr, round_up_fast(size, T::ALIGN)) };
            self.len_elements -= 1;
            Some(t)
        } else {
            None
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len_elements, Some(self.len_elements))
    }

    #[inline]
    fn count(self) -> usize {
        self.len_elements
    }
}

impl<'a, T: VarLen> ExactSizeIterator for Iter<'a, T> {
    #[inline]
    fn len(&self) -> usize {
        self.len_elements
    }
}

/// Iterates over mutable references.
pub struct IterMut<'a, T> {
    ptr: NonNull<T>,
    len_elements: usize,
    marker: core::marker::PhantomData<&'a mut [T]>,
}

impl<'a, T: VarLen> IterMut<'a, T> {
    /// Returns a copy of this iterator that is limited to at most `n` elements.
    ///
    /// This is a slightly more efficient variant of `take`.
    #[inline]
    pub fn limited_to(mut self, n: usize) -> Self {
        self.len_elements = core::cmp::min(self.len_elements, n);
        self
    }
}

impl<'a, T: VarLen> Iterator for IterMut<'a, T> {
    type Item = Pin<&'a mut T>;

    #[inline]
    fn next(&mut self) -> Option<Pin<&'a mut T>> {
        if self.len_elements > 0 {
            let t = unsafe { Pin::new_unchecked(self.ptr.as_mut()) };
            let size = t.calculate_layout().size();
            self.ptr = unsafe { add_bytes_fast(self.ptr, round_up_fast(size, T::ALIGN)) };
            self.len_elements -= 1;
            Some(t)
        } else {
            None
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len_elements, Some(self.len_elements))
    }

    #[inline]
    fn count(self) -> usize {
        self.len_elements
    }
}

impl<'a, T: VarLen> ExactSizeIterator for IterMut<'a, T> {
    #[inline]
    fn len(&self) -> usize {
        self.len_elements
    }
}

/// Iterates over owned elements of a Seq. Walking this iterator takes ownership of the
/// elements, but ownership of the underlying byte storage remains with the Seq.
pub struct OwnedElems<'a, T: VarLen> {
    // Modified by iterator:
    ptr: NonNull<T>,
    len_elements: usize,
    marker: core::marker::PhantomData<&'a [u8]>,
}

impl<'a, T: VarLen> Iterator for OwnedElems<'a, T> {
    type Item = Owned<'a, T>;

    #[inline]
    fn next(&mut self) -> Option<Owned<'a, T>> {
        if self.len_elements > 0 {
            let t = unsafe { Owned::from_raw(self.ptr) };
            let size = T::calculate_layout(&*t).size();
            self.ptr = unsafe { add_bytes_fast(self.ptr, round_up_fast(size, T::ALIGN)) };
            self.len_elements -= 1;
            Some(t)
        } else {
            None
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len_elements, Some(self.len_elements))
    }

    #[inline]
    fn count(self) -> usize {
        self.len_elements
    }
}

impl<'a, T: VarLen> ExactSizeIterator for OwnedElems<'a, T> {
    #[inline]
    fn len(&self) -> usize {
        self.len_elements
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

#[cfg(test)]
mod tests {
    #[test]
    fn push_box() {
        use crate::{Seq, VBox, Str};
        let s: VBox<Str> = VBox::new(Str::copy_from_str("hello"));
        let mut seq = Seq::new_sequential();
        seq.push(s);
    }
}