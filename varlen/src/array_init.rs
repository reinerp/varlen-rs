#![warn(missing_docs)]
#![warn(rustdoc::missing_doc_code_examples)]


//! An [`ArrayInitializer<T>`] is an object that knows how to initialize the memory for a `[T]`.
//! It is useful for initializing `[T; N]` or (of more relevance for this crate) initializing
//! a `varlen::array::Array<T>`.
//! 
//! # Examples
//! 
//! Initializing a `[T; N]` from an initializer:
//! 
//! ```
//! use varlen::array_init;
//! 
//! let arr: [u16; 4] = array_init::new_array(array_init::FillWithDefault);
//! assert_eq!([0, 0, 0, 0], arr);
//! 
//! let arr2: [u16; 4] = array_init::new_array(array_init::FillSequentially(|i| (i * 2) as u16));
//! assert_eq!([0, 2, 4, 6], arr2);
//! ```
//! 
//! Initializing a `varlen::array::Array<T>` from an initializer:
//! 
//! ```
//! use varlen::array_init;
//! use varlen::array::{Array, SizedInit};
//! use varlen::vbox::VBox;
//! 
//! let arr: VBox<Array<u16>> = VBox::new(SizedInit(4, array_init::FillSequentially(|i| (i * 2) as u16)));
//! assert_eq!(&[0, 2, 4, 6], &arr[..]);
//! ```

use core::mem::MaybeUninit;
use core::ptr::NonNull;

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
/// use varlen::array_init;
/// 
/// let arr: [u16; 4] = array_init::new_array(array_init::FillSequentially(|i| (i * 2) as u16));
/// assert_eq!([0, 2, 4, 6], arr);
/// ```
pub fn new_array<const N: usize, T>(init: impl ArrayInitializer<T>) -> [T; N] {
    let mut data= [HasUninit::UNINIT; N];
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
/// use varlen::array_init::ArrayInitializer;
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
    fn initialize(self, dst: &mut [MaybeUninit<T>]);
}

// /// Substitute for NonNull::as_uninit_slice_mut() until that stabilizes.
// pub unsafe fn as_uninit_slice_mut<'a, T>(p: NonNull<[T]>) -> &'a mut [MaybeUninit<T>] {
//     NonNull::new_unchecked(p.as_ptr() as *mut [MaybeUninit<T>]).as_mut()
// }

pub struct FromIterPrefix<Iter>(pub Iter);

unsafe impl<T, Iter: Iterator<Item = T>> ArrayInitializer<T> for FromIterPrefix<Iter> {
    fn initialize(mut self, dst: &mut [MaybeUninit<T>]) {
        for slot in dst.iter_mut() {
            slot.write(self.0.next().unwrap());
        }
    }
}

pub struct FillSequentially<Lambda>(pub Lambda);

unsafe impl<T, Lambda: FnMut(usize) -> T> ArrayInitializer<T> for FillSequentially<Lambda> {
    fn initialize(mut self, dst: &mut [MaybeUninit<T>]) {
        for (i, slot) in dst.iter_mut().enumerate() {
            slot.write(self.0(i));
        }
    }
}

pub struct FillWithDefault;

unsafe impl<T: Default> ArrayInitializer<T> for FillWithDefault {
    fn initialize(self, dst: &mut [MaybeUninit<T>]) {
        FillSequentially(|_i| Default::default()).initialize(dst)
    }
}

pub struct CopyFrom<'a, T>(pub &'a [T]);

unsafe impl<'a, T: Copy> ArrayInitializer<T> for CopyFrom<'a, T> {
    fn initialize(self, dst: &mut [MaybeUninit<T>]) {
        // TODO(reinerp): Use MaybeUninit::write_slice once it stabilizes.
        // Safety: MaybeUninit<T> and T have the same layout.
        let src: &[MaybeUninit<T>] = unsafe { core::mem::transmute(self.0) };
        dst.copy_from_slice(src)
    }
}

pub struct CloneFrom<'a, T>(pub &'a [T]);

unsafe impl<'a, T: Clone> ArrayInitializer<T> for CloneFrom<'a, T> {
    fn initialize(self, dst: &mut [MaybeUninit<T>]) {
        assert_eq!(self.0.len(), dst.len());
        for (src, dst) in self.0.iter().zip(dst.iter_mut()) {
            dst.write(src.clone());
        }
    }
}

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