//! An [`ArrayInitializer<T>`] is an object that knows how to initialize the memory for a `[T]`.
//! It is useful for initializing `[T; N]` or (of more relevance for this crate) initializing
//! a `varlen::array::Array<T>`.
//!
//! # Examples
//!
//! Initializing a `[T; N]` from an initializer:
//!
//! ```
//! use varlen::prelude::*;
//!
//! let arr: [u16; 4] = new_array(FillWithDefault);
//! assert_eq!([0, 0, 0, 0], arr);
//!
//! let arr2: [u16; 4] = new_array(FillSequentially(|i| (i * 2) as u16));
//! assert_eq!([0, 2, 4, 6], arr2);
//! ```
//!
//! Initializing a `varlen::array::Array<T>` from an initializer:
//!
//! ```
//! use varlen::prelude::*;
//!
//! let arr: VBox<Array<u16>> = VBox::new(SizedInit(4, FillSequentially(|i| (i * 2) as u16)));
//! assert_eq!(&[0, 2, 4, 6], &arr[..]);
//! ```

use core::mem::MaybeUninit;

trait HasUninit: Sized {
    const UNINIT: MaybeUninit<Self>;
}

impl<T> HasUninit for T {
    const UNINIT: MaybeUninit<Self> = MaybeUninit::uninit();
}

/// Initializes an array from an initializer.
///
/// # Examples
///
/// ```
/// use varlen::prelude::*;
///
/// let arr: [u16; 4] = new_array(FillSequentially(|i| (i * 2) as u16));
/// assert_eq!([0, 2, 4, 6], arr);
/// ```
pub fn new_array<T, const N: usize>(init: impl ArrayInitializer<T>) -> [T; N] {
    let mut data = [HasUninit::UNINIT; N];
    init.initialize(&mut data);
    unsafe {
        // Safety:
        // * guaranteed to be initialized by safety requirement on ArrayInitializer
        // * size and alignment are guaranteed to match between [MaybeUninit<T>; N]
        //   and [T; N]
        transmute_workaround(data)
    }
}

/// An object that is able to initialize an array of `T` values.
///
/// # Examples
///
/// An initializer that fills an array from end to start:
///
/// ```
/// use varlen::prelude::*;
/// use std::mem::MaybeUninit;
///
/// struct WriteBackwardsPowersOf3;
/// unsafe impl ArrayInitializer<u64> for WriteBackwardsPowersOf3 {
///     fn initialize(self, dst: &mut [MaybeUninit<u64>]) {
///         let mut v = 1;
///         for slot in dst.iter_mut().rev() {
///             slot.write(v);
///             v *= 3;
///         }
///     }
/// }
///
/// assert_eq!([81, 27, 9, 3, 1], varlen::array_init::new_array(WriteBackwardsPowersOf3));
/// ```
pub unsafe trait ArrayInitializer<T> {
    /// Fills the slice.
    ///
    /// ```
    /// use std::mem::MaybeUninit;
    /// use varlen::prelude::*;
    /// const UNINIT_U16: MaybeUninit<u16> = MaybeUninit::uninit();
    /// let mut arr = [UNINIT_U16; 4];
    /// FillWithDefault.initialize(&mut arr[..]);
    /// for slot in arr {
    ///     assert_eq!(0, unsafe { slot.assume_init() });
    /// }
    /// ```
    fn initialize(self, dst: &mut [MaybeUninit<T>]);
}

/// Fills an array from a prefix of an iterator.
///
/// # Example
///
/// ```
/// use varlen::prelude::*;
/// let iter = std::iter::successors(Some(3), |i| Some(i * 3));
/// let arr: [u16; 5] = new_array(FromIterPrefix(iter));
/// assert_eq!(arr, [3, 9, 27, 81, 243]);
/// ```
///
/// # Panics
///
/// Panics if the iterator yields fewer elements than the desired output size.
///
/// ```should_panic
/// use varlen::prelude::*;
/// let iter = std::iter::successors(Some(3), |i| Some(i * 3));
/// let arr: [u16; 5] = new_array(FromIterPrefix(iter.take(3))); // Panics
/// ```
pub struct FromIterPrefix<Iter>(pub Iter);

unsafe impl<T, Iter: Iterator<Item = T>> ArrayInitializer<T> for FromIterPrefix<Iter> {
    fn initialize(mut self, dst: &mut [MaybeUninit<T>]) {
        for slot in dst.iter_mut() {
            slot.write(
                self.0
                    .next()
                    .unwrap_or_else(|| panic!("Iterator had too few elements")),
            );
        }
    }
}

/// Fills an array by calling the function for every element, in ascending order by index.
///
/// The function is given the `usize` element index as a parameter.
///
/// # Examples
///
/// ```
/// use varlen::prelude::*;
/// let arr: [u16; 5] = new_array(FillSequentially(|i| (i as u16) * 2));
/// assert_eq!(arr, [0, 2, 4, 6, 8]);
/// ```
///
/// Stateful functions are also allowed:
///
/// ```
/// use varlen::prelude::*;
/// let mut state = 1;
/// let arr: [u16; 5] = new_array(FillSequentially(|_| {
///     state = state * 3;
///     state
/// }));
/// assert_eq!(arr, [3, 9, 27, 81, 243]);
/// ```
pub struct FillSequentially<Lambda>(pub Lambda);

unsafe impl<T, Lambda: FnMut(usize) -> T> ArrayInitializer<T> for FillSequentially<Lambda> {
    fn initialize(mut self, dst: &mut [MaybeUninit<T>]) {
        for (i, slot) in dst.iter_mut().enumerate() {
            slot.write(self.0(i));
        }
    }
}

/// Fills an array with [`Default::default()`].
///
/// # Examples
///
/// ```
/// use varlen::prelude::*;
/// let arr: [u16; 5] = new_array(FillWithDefault);
/// assert_eq!(arr, [0, 0, 0, 0, 0]);
/// ```
pub struct FillWithDefault;

unsafe impl<T: Default> ArrayInitializer<T> for FillWithDefault {
    fn initialize(self, dst: &mut [MaybeUninit<T>]) {
        FillSequentially(|_i| Default::default()).initialize(dst)
    }
}

/// Fills an array by copying from a source array. Source and destination must have identical length.
///
/// # Examples
///
/// ```
/// use varlen::prelude::*;
/// let arr: [u16; 5] = new_array(CopyFrom(&[3, 1, 4, 1, 5]));
/// assert_eq!(arr, [3, 1, 4, 1, 5]);
/// ```
///
/// # Panics
///
/// Panics if the source and destination arrays have different length.
///
/// ```should_panic
/// use varlen::prelude::*;
/// let arr: [u16; 6] = new_array(CopyFrom(&[3, 1, 4, 1, 5]));  // Panic
/// ```
///
/// ```should_panic
/// use varlen::prelude::*;
/// let arr: [u16; 4] = new_array(CopyFrom(&[3, 1, 4, 1, 5]));  // Panic
/// ```
pub struct CopyFrom<'a, T>(pub &'a [T]);

unsafe impl<'a, T: Copy> ArrayInitializer<T> for CopyFrom<'a, T> {
    fn initialize(self, dst: &mut [MaybeUninit<T>]) {
        // TODO(reinerp): Use MaybeUninit::write_slice once it stabilizes.
        // Safety: MaybeUninit<T> and T have the same layout.
        let src: &[MaybeUninit<T>] = unsafe { core::mem::transmute(self.0) };
        dst.copy_from_slice(src)
    }
}

/// Fills an array by cloning from a source array. Source and destination must have identical length.
///
/// # Examples
///
/// ```
/// use varlen::prelude::*;
///
/// let src = ["hello".to_string(), "world".to_string()];
/// let arr: [String; 2] = new_array(CloneFrom(&src));
/// assert_eq!(src, arr);
/// ```
///
/// # Panics
///
/// Panics if the source and destination have different lengths.
///
/// ```should_panic
/// # use varlen::prelude::*;
/// #
/// # let src = ["hello".to_string(), "world".to_string()];
/// let arr: [String; 1] = new_array(CloneFrom(&src));  // Panics
/// ```
pub struct CloneFrom<'a, T>(pub &'a [T]);

unsafe impl<'a, T: Clone> ArrayInitializer<T> for CloneFrom<'a, T> {
    fn initialize(self, dst: &mut [MaybeUninit<T>]) {
        assert_eq!(self.0.len(), dst.len());
        for (src, dst) in self.0.iter().zip(dst.iter_mut()) {
            dst.write(src.clone());
        }
    }
}

/// Fills an array by moving from a source array. Source and destination must have identical length.
///
/// # Examples
///
/// ```
/// use varlen::prelude::*;
///
/// let src = ["hello".to_string(), "world".to_string()];
/// let arr: [String; 2] = new_array(MoveFrom(src));
/// assert_eq!(arr, ["hello".to_string(), "world".to_string()]);
/// ```
///
/// # Panics
///
/// Panics if the source and destination have different lengths.
///
/// ```should_panic
/// # use varlen::prelude::*;
/// #
/// # let src = ["hello".to_string(), "world".to_string()];
/// let arr: [String; 1] = new_array(MoveFrom(src));  // Panics
/// ```
pub struct MoveFrom<T, const N: usize>(pub [T; N]);

unsafe impl<T, const N: usize> ArrayInitializer<T> for MoveFrom<T, N> {
    fn initialize(self, dst: &mut [MaybeUninit<T>]) {
        assert_eq!(dst.len(), N);
        for (src, dst) in self.0.into_iter().zip(dst.iter_mut()) {
            dst.write(src);
        }
    }
}

/// Same as `transmute`, but supports types whose sizes are not known until monomorphization.
unsafe fn transmute_workaround<T, U>(e: T) -> U {
    assert!(core::alloc::Layout::new::<T>() == core::alloc::Layout::new::<U>());
    let u = core::mem::transmute_copy(&e);
    core::mem::forget(e);
    u
}
