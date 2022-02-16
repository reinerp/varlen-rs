#![warn(missing_docs)]
#![warn(rustdoc::missing_doc_code_examples)]

//! Equivalent of [`Box<T>`] for variable-length types.
//!
//! # Examples
//!
//! Heap-allocated `Str`:
//!
//! ```
//! use varlen::prelude::*;
//! let s = VBox::new(Str::copy_from_str("hello"));
//! assert_eq!("hello", &s[..]);
//! ```

use crate::VClone;

use super::{Initializer, Layout, VarLen};

use core::alloc;
use core::pin::Pin;
use core::ptr::NonNull;

/// Equivalent of [`Box<T>`] for variable-length types.
///
/// # Examples
///
/// Heap-allocated `Str`:
///
/// ```
/// use varlen::prelude::*;
/// let s = VBox::new(Str::copy_from_str("hello"));
/// assert_eq!("hello", &s[..]);
/// ```
pub struct VBox<T: VarLen>(NonNull<T>);

#[inline(never)]
#[cold]
fn allocation_overflow() -> ! {
    panic!("Allocation size overflow")
}

#[allow(rustdoc::missing_doc_code_examples)]
impl<T: VarLen> VBox<T> {
    /// Allocates memory which is right-sized for this particular instance.
    ///
    /// # Examples
    ///
    /// ```
    /// use varlen::prelude::*;
    /// let s = VBox::new(Str::copy_from_str("hello"));
    /// assert_eq!("hello", &s[..]);
    /// ```
    pub fn new(init: impl Initializer<T>) -> Self {
        let layout = init
            .calculate_layout_cautious()
            .unwrap_or_else(|| allocation_overflow());
        let alloc_layout = alloc::Layout::from_size_align(layout.size(), T::ALIGN)
            .unwrap_or_else(|_| allocation_overflow());
        unsafe {
            let p = std::alloc::alloc(alloc_layout) as *mut T;
            let layout_size = layout.size();
            init.initialize(NonNull::new_unchecked(p), layout);
            let mut p = NonNull::new_unchecked(p);
            // TODO(reinerp): Compare directly on Layout type? Or too much generated code?
            debug_assert_eq!(p.as_mut().calculate_layout().size(), layout_size);
            VBox(p)
        }
    }

    /// Mutable access to the field.
    ///
    /// # Examples
    ///
    /// ```
    /// use varlen::prelude::*;
    /// let mut s = VBox::new(Str::copy_from_str("Hello"));
    /// assert_eq!("Hello", &s[..]);
    /// s.as_mut().mut_slice().make_ascii_uppercase();
    /// assert_eq!("HELLO", &s[..]);
    /// s.as_mut().mut_slice().make_ascii_lowercase();
    /// assert_eq!("hello", &s[..]);
    /// ```
    pub fn as_mut(&mut self) -> Pin<&mut T> {
        unsafe { Pin::new_unchecked(self.0.as_mut()) }
    }

    /// Converts this to a raw pointer representation.
    ///
    /// # Safety
    ///
    /// Because `T` is a variable-length type, there are additional safety obligations
    /// above and beyond the usual treatment of `NonNull<T>`. In particular, the caller
    /// responsible for ensuring that whenever a `&T` is produced, the header-specified
    /// layout matches the layout of the tail. This prohibits code patterns such as
    /// overwriting the header in a way that changes the layout.
    ///
    /// # Example
    ///
    /// Safe roundtripping through a raw pointer:
    ///
    /// ```
    /// use varlen::prelude::*;
    ///
    /// let b = VBox::new(Str::copy_from_str("hello"));
    /// let b = unsafe {
    ///     let p = b.into_raw();
    ///     VBox::from_raw(p)
    /// };
    /// assert_eq!(&b[..], "hello");
    /// ```
    pub unsafe fn into_raw(self) -> *mut T {
        let result = self.0.as_ptr();
        core::mem::forget(self);
        result
    }

    /// Constructs a [`VBox<T>`] from a [`NonNull<T>`] pointer.
    ///
    /// # Safety
    ///
    /// The layout of `T`'s _tail_, which is the variable-sized part not included in
    /// [`std::mem::size_of::<T>()`], must be consistent with the layout specified by
    /// `T`'s header. For example, this can have been produced by a
    /// [`crate::Initializer<T>`] call or similar, on a buffer sufficiently sized for
    /// the initializer's layout.
    ///
    /// # Example
    ///
    /// Safe roundtripping through a raw pointer:
    ///
    /// ```
    /// use varlen::prelude::*;
    ///
    /// let b = VBox::new(Str::copy_from_str("hello"));
    /// let b = unsafe {
    ///     let p = b.into_raw();
    ///     VBox::from_raw(p)
    /// };
    /// assert_eq!("hello", &b[..]);
    /// ```
    pub unsafe fn from_raw(raw: *mut T) -> Self {
        VBox(NonNull::new_unchecked(raw))
    }
}

impl<T: VarLen> Drop for VBox<T> {
    fn drop(&mut self) {
        unsafe {
            // Careful sequencing of drop:
            //  1. Read the layout, before we drop it.
            //  2. Drop the header. Needs to happen before dropping the tail, because there might
            //     be a custom Drop on the header that reads the tail.
            //  3. Drop the tail. Uses the values we read from the header in step 1.
            //  4. Deallocate.
            let layout = T::calculate_layout(&*self);
            let alloc_layout = alloc::Layout::from_size_align_unchecked(layout.size(), T::ALIGN);
            let p = self.0.as_ptr();
            core::ptr::drop_in_place(p);
            T::drop_tail(self.as_mut(), layout);
            std::alloc::dealloc(p as *mut u8, alloc_layout);
        }
    }
}

impl<T: VarLen> core::ops::Deref for VBox<T> {
    type Target = T;
    #[inline]
    fn deref(&self) -> &T {
        unsafe { self.0.as_ref() }
    }
}

/// [`VBox<T>`] is an initializer for `T`.
///
/// # Examples
///
/// Pushing a [`VBox<T>`] onto a [`crate::seq::Seq<T>`]:
///
/// ```
/// use varlen::prelude::*;
///
/// let mut seq: Seq<Str> = Seq::new();
/// let b = VBox::new(Str::copy_from_str("hello"));
/// seq.push(b);
/// ```
unsafe impl<T: VarLen> Initializer<T> for VBox<T> {
    unsafe fn initialize(self, dst: NonNull<T>, layout: T::Layout) {
        let VBox(ptr) = self;
        let ptr = ptr.as_ptr().cast::<u8>();
        let size = layout.size();
        // Safety: we already called from_size_align in the VBox constructor.
        let layout = alloc::Layout::from_size_align_unchecked(size, T::ALIGN);
        // Safety:
        //  * Owned has unique access to its pointer
        //  * dst is unique
        //  * dst size is guaranteed by the SizedInitializer call
        core::ptr::copy_nonoverlapping(ptr, dst.as_ptr().cast::<u8>(), size);
        std::alloc::dealloc(ptr, layout);
        core::mem::forget(self);
    }

    #[inline]
    fn calculate_layout_cautious(&self) -> Option<T::Layout> {
        Some(T::calculate_layout(&*self))
    }
}

/// Cloning a [`VBox<T>`] uses `T::vclone()`.
///
/// # Examples
///
/// ```
/// use varlen::prelude::*;
/// let str: VBox<Str> = VBox::new(Str::copy_from_str("hello"));
/// let str2 = str.clone();
/// assert_eq!(&str2[..], "hello");
/// ```
///
/// # See also
///
/// It is often better to use [`T::vclone()`](crate::VClone::vclone) or
/// [`vcopy()`](crate::VCopy::vcopy) instead, which will create a lazy
/// initializer that directly clones or copies into the destination.
///
/// ```
/// use varlen::prelude::*;
/// let str: VBox<Str> = VBox::new(Str::copy_from_str("hello"));
/// let seq: Seq<Str> = seq![
///     // Best. Calls memcpy straight into the sequence storage
///     str.vcopy(),   
///     // Ok. Does field-by-field copy into the sequence storage.
///     str.vclone(),  
///     // Worst. Allocates a temporary VBox<Str>, copies field-by-field to there,
///     // then coies field-by-field to the sequence storage, then deallocates the
///     // temporary.
///     str.clone(),   
/// ];
/// let mut iter = seq.iter();
/// assert_eq!(&iter.next().unwrap()[..], "hello");
/// assert_eq!(&iter.next().unwrap()[..], "hello");
/// assert_eq!(&iter.next().unwrap()[..], "hello");
/// assert!(iter.next().is_none());
/// ```
impl<T: for<'a> VClone<'a>> Clone for VBox<T> {
    fn clone(&self) -> Self {
        VBox::new(self.vclone())
    }
}
