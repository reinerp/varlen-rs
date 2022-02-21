//! A pointer to `T` that calls its destructor but not its deallocator when dropped.
//!
//! # Examples
//!
//! ```
//! # #[cfg(feature = "bumpalo")] {
//! use varlen::prelude::*;
//! type TypeWithDrop = Tup2<FixedLen<Box<u32>>, Str>;
//! let arena = bumpalo::Bump::new();
//! let owned: Owned<TypeWithDrop> = Owned::new_in(
//!     tup2::Init(
//!         FixedLen(Box::new(42)),
//!         Str::copy("hello")
//!     ),
//!     &arena);
//! assert_eq!(42, *owned.refs().0.0);
//! drop(owned); // Calls drop() on the Box<u32>
//! drop(arena); // Deallocates the arena storage.
//! # }
//! ```
use crate::Initializer;

use super::{Layout, VarLen};

use core::marker::PhantomData;
use core::pin::Pin;
use core::ptr::NonNull;

/// A pointer to `T` that calls its destructor but not its deallocator when dropped.
///
/// # Examples
///
/// ```
/// #[cfg(feature = "bumpalo")] {
/// use varlen::prelude::*;
/// type TypeWithDrop = Tup2<FixedLen<Box<u32>>, Str>;
/// let arena = bumpalo::Bump::new();
/// let owned: Owned<TypeWithDrop> = Owned::new_in(
///     tup2::Init(
///         FixedLen(Box::new(42)),
///         Str::copy("hello")
///     ),
///     &arena);
/// assert_eq!(42, *owned.refs().0.0);
/// drop(owned); // Calls drop() on the Box<u32>
/// drop(arena); // Deallocates the arena storage.
/// }
/// ```
///
/// # Comparison to `T` on fixed length types
///
/// The type [`Owned<T>`] fills a role for variable-length types similar to what `T` fills
/// for fixed-length types, typically in "into" or "take" APIs. For example, whereas
/// `Vec<T>::into_iter` iterates over `T` items (taking them and potentially dropping them
/// one-by-one), [`crate::seq::Seq<T>::take_elems`] iterates over `Owned<T>` items, taking them and
/// potentially dropping them one-by-bone:
///
/// ```
/// use varlen::prelude::*;
/// fn consume_seq<T: VarLen>(mut seq: Seq<T>) {
///     for t in seq.take_elems() {
///         let t: Owned<T> = t;
///         // T's destructor runs here, freeing any memory it points to.
///     }
///     assert_eq!(0, seq.len());
///     // Seq<T>'s destructor runs here, freeing its underlying storage.
/// }
/// ```
pub struct Owned<'storage, T: VarLen>(NonNull<T>, PhantomData<&'storage [u8]>);

#[allow(rustdoc::missing_doc_code_examples)]
impl<'storage, T: VarLen> Owned<'storage, T> {
    /// Constructs an `Owned<T>` pointer from a `NonNull<T>` pointer.
    ///
    /// # Safety
    ///
    /// The layout of `T`'s _tail_, which is the variable-sized part not included in
    /// `std::mem::size_of::<T>()`, must be consistent with the layout specified by
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
    /// fn roundtrip<T: VarLen>(x: Owned<T>) -> Owned<T> {
    ///     unsafe {
    ///         let p = x.into_raw();
    ///         Owned::from_raw(p)
    ///     }
    /// }
    /// ```
    pub unsafe fn from_raw(raw: NonNull<T>) -> Self {
        Owned(raw, PhantomData)
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
    /// fn roundtrip<T: VarLen>(x: Owned<T>) -> Owned<T> {
    ///     unsafe {
    ///         let p = x.into_raw();
    ///         Owned::from_raw(p)
    ///     }
    /// }
    /// ```
    pub unsafe fn into_raw(self) -> NonNull<T> {
        let result = self.0;
        core::mem::forget(self);
        result
    }

    /// Gets a (pinned) mutable reference.
    ///
    /// # Example
    ///
    /// ```
    /// # #[cfg(feature = "bumpalo")] {
    /// use varlen::prelude::*;
    /// use bumpalo::Bump;
    ///
    /// let arena = Bump::new();
    /// let mut s = Owned::new_in(Str::copy("Hello"), &arena);
    /// s.as_mut().mut_slice().make_ascii_uppercase();
    /// assert_eq!("HELLO", &s[..]);
    /// s.as_mut().mut_slice().make_ascii_lowercase();
    /// assert_eq!("hello", &s[..]);
    /// # }
    /// ```
    pub fn as_mut(&mut self) -> Pin<&mut T> {
        unsafe { Pin::new_unchecked(self.0.as_mut()) }
    }

    /// Forgets the obligation to drop this.
    ///
    /// # Example
    ///
    /// ```
    /// # #[cfg(feature = "bumpalo")] {
    /// use bumpalo::Bump;
    /// use varlen::prelude::*;
    /// let arena = Bump::new();
    /// let s = Owned::new_in(Str::copy("hello"), &arena);
    /// let s = s.leak();
    /// assert_eq!("hello", &s[..]);
    /// # }
    /// ```
    pub fn leak(self) -> Pin<&'storage mut T> {
        let Owned(mut ptr, _) = self;
        unsafe { Pin::new_unchecked(ptr.as_mut()) }
    }

    /// Allocates a `T` on the arena.
    ///
    /// # Example
    ///
    /// ```
    /// # #[cfg(feature = "bumpalo")] {
    /// use bumpalo::Bump;
    /// use varlen::prelude::*;
    /// let arena = Bump::new();
    /// let s = Owned::new_in(Str::copy("hello"), &arena);
    /// assert_eq!("hello", &s[..]);
    /// # }
    /// ```
    #[cfg(feature = "bumpalo")]
    #[inline]
    pub fn new_in(init: impl Initializer<T>, bump: &'storage bumpalo::Bump) -> Self {
        use core::alloc;
        let layout = init.calculate_layout_cautious().unwrap();
        let ptr = bump
            .alloc_layout(alloc::Layout::from_size_align(layout.size(), T::ALIGN).unwrap())
            .cast::<T>();
        unsafe {
            init.initialize(ptr, layout);
        }
        Owned(ptr, PhantomData)
    }
}

impl<T: VarLen> Drop for Owned<'_, T> {
    fn drop(&mut self) {
        unsafe {
            // Safety: vdrop is called only once, because Self::drop() is called only once.
            let layout = T::calculate_layout(&*self);
            T::vdrop(self.as_mut(), layout);
        }
    }
}

impl<T: VarLen> core::ops::Deref for Owned<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        unsafe { self.0.as_ref() }
    }
}

unsafe impl<T: VarLen> Initializer<T> for Owned<'_, T> {
    unsafe fn initialize(self, dst: NonNull<T>, layout: T::Layout) {
        // Safety:
        //  * Owned has unique access to its pointer
        //  * dst is unique
        //  * dst size is guaranteed by the SizedInitializer call
        core::ptr::copy_nonoverlapping(self.0.as_ptr(), dst.as_ptr(), layout.size());
        core::mem::forget(self);
    }

    #[inline]
    fn calculate_layout_cautious(&self) -> Option<T::Layout> {
        Some(T::calculate_layout(&*self))
    }
}
