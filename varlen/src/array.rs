#![warn(missing_docs)]
#![warn(rustdoc::missing_doc_code_examples)]
#![doc = crate::make_svgbobdoc!(
//! A variable-length array with inline storage. 
//! 
//! This consists of an integer `length` field, followed immediately (subject to alignment) by the
//! array payload of the specified length. For example, the `VBox<Array<u8>>` representation of `[10u8, 11, 12]` is:
//! 
//! ```svgbob
//! "VBox"
//! +----------+
//! | ptr      |
//! +----------+
//!      |
//!      |   Array
//!      |   +-----------+----+----+----+
//!      '-> | 3: usize  | 10 | 11 | 12 |       
//!          +-----------+----+----+----+
//! ```
//!
//! Once allocated, the array size may not be modified.
//! 
//! # Examples
//! 
//! ```
//! # use varlen::{VBox, Array};
//! # use crate::varlen::{Layout, VarLen};
//! let a = VBox::new(Array::copy_from_slice(&[1u8, 2, 3]));
//! assert_eq!(&a[..], &[1, 2, 3]);
//! // Layout is as specified above:
//! assert_eq!(a.calculate_layout().size(), std::mem::size_of::<usize>() + 3)
//! ```
//! 
//! # Module contents
//! 
//! The main type is [`Array<T>`], valid length types for the array are [`ArrayLen`], its memory layout is
//! calculated and stored in [`ArrayLayout`], and its general initializer is [`SizedInit`].
)]
use crate::marker::ArrayMarker;
use crate::{Initializer, Layout, VarLen};
use crate::array_init::ArrayInitializer;
use core::pin::Pin;

#[doc = crate::make_svgbobdoc!(
/// An variable-length array with inline storage.
/// 
/// This consists of an integer length field, followed immediately (subject to alignment) by the
/// array payload of the specified length. For example, the [`crate::VBox<Array<u8>>`] representation of `[10u8, 11, 12]` is:
/// 
/// ```svgbob
/// "VBox"
/// +----------+
/// | ptr      |
/// +----------+
///      |
///      |   Array
///      |   +-----------+----+----+----+
///      '-> | 3: usize  | 10 | 11 | 12 |       
///          +-----------+----+----+----+
/// ```
/// 
/// # Examples
/// 
/// The `Array` type cannot be stored on the stack, because its size is not known at compile time.
/// Instead, store it in a type such as [`crate::VBox`] or [`crate::Seq`]:
/// 
/// ```
/// use varlen::{VBox, Array};
/// let mut a: VBox<Array<u16>> = VBox::new(Array::copy_from_slice(&[1, 2, 3]));
/// assert_eq!(&a[..], &[1, 2, 3]);
/// a.as_mut().mut_slice()[2] = 5;
/// assert_eq!(&a[..], &[1, 2, 5]);
/// ```
/// 
/// # Smaller `length` field
/// 
/// The `Len` type parameter controls what integer type is used to store the array length. This
/// defaults to `usize`, which is long enough to express any array that fits in memory. If this
/// integer type is needlessly large, you may specify a smaller integer type to store the length,
/// at the expense of not supporting longer arrays:
/// 
/// ```
/// use varlen::{VBox, Array};
/// // Construction succeeds for short arrays:
/// let arr: VBox<Array<u16, u8>> = VBox::new(Array::try_copy_from_slice(&[1, 2, 3]).unwrap());
/// assert_eq!(&arr[..], &[1, 2, 3]);
/// // Construction fails for arrays that are too long:
/// assert!(Array::<u16, u8>::try_copy_from_slice(&[1u16; 257]).is_none());
/// ```
/// 
/// # Layout
/// 
/// The type `Array` uses just enough storage for the length, the payload, plus padding alignment:
/// 
/// ```
/// use varlen::{VBox, Array, VarLen, Layout};
/// let arr: VBox<Array<u8, u8>> = VBox::new(Array::try_copy_from_slice(&[1, 2, 3]).unwrap());
/// assert_eq!(1 + 3, arr.calculate_layout().size()); 
/// ```
)]
pub struct Array<T, Len: ArrayLen = usize> {
    len: Len,
    _array: ArrayMarker<T>,
}

/// The memory layout of an [`Array`].
///
/// # Examples
///
/// ```
/// use varlen::{VBox, Array, VarLen, Layout};
/// let arr: VBox<Array<u8, u8>> = VBox::new(Array::try_copy_from_slice(&[1, 2, 3]).unwrap());
/// assert_eq!(1 + 3, arr.calculate_layout().size());
/// ```

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
        let (array_offset, array_len, size) =
            crate::macro_support::cat_array_field_fast::<T>(self.len.as_usize(), offset);
        ArrayLayout {
            array_offset,
            array_len,
            size,
        }
    }

    const ALIGN: usize = crate::macro_support::array_max(&[
        core::mem::align_of::<Self>(),
        core::mem::align_of::<T>(),
    ]);
    const NEEDS_DROP_TAIL: bool = core::mem::needs_drop::<T>();

    #[inline(always)]
    unsafe fn drop_tail(self: core::pin::Pin<&mut Self>, layout: Self::Layout) {
        crate::macro_support::drop_array::<T>(
            self.get_unchecked_mut() as *mut _ as *mut u8,
            layout.array_offset,
            layout.array_len,
        );
    }
}

impl<T, Len: ArrayLen> core::ops::Deref for Array<T, Len> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        let layout = self.calculate_layout();
        unsafe { crate::macro_support::ref_array(self, layout.array_offset, layout.array_len) }
    }
}

#[allow(rustdoc::missing_doc_code_examples)]
impl<T> Array<T> {
    /// Initializes an [`Array`] by copying from an existing slice.
    ///
    /// # Examples
    ///
    /// ```
    /// # use varlen::{VBox, Array};
    /// let a = VBox::new(Array::copy_from_slice(&[1, 2, 3]));
    /// assert_eq!(&a[..], &[1, 2, 3]);
    /// ```
    pub fn copy_from_slice(src: &[T]) -> SizedInit<crate::array_init::CopyFrom<T>>
    where
        T: Copy,
    {
        Array::try_copy_from_slice(src).unwrap()
    }

    /// Initializes an [`Array`] by cloning from an existing slice.
    ///
    /// # Examples
    ///
    /// ```
    /// # use varlen::{VBox, Array};
    /// let a: VBox<Array<Box<u16>>> = VBox::new(Array::clone_from_slice(&[Box::new(1), Box::new(2)]));
    /// assert_eq!(&a[..], &[Box::new(1), Box::new(2)]);
    /// ```
    pub fn clone_from_slice(src: &[T]) -> SizedInit<crate::array_init::CloneFrom<T>>
    where
        T: Clone,
    {
        Array::try_clone_from_slice(src).unwrap()
    }
}

#[allow(rustdoc::missing_doc_code_examples)]
impl<T, Len: ArrayLen> Array<T, Len> {
    /// Gives mutable access to the payload.
    ///
    /// # Examples
    ///
    /// ```
    /// # use varlen::{VBox, Array};
    /// let mut a = VBox::new(Array::copy_from_slice(&[1, 2, 3]));
    /// a.as_mut().mut_slice()[2] = 5;
    /// assert_eq!(&a[..], &[1, 2, 5]);
    /// ```
    pub fn mut_slice(self: Pin<&mut Self>) -> &mut [T] {
        let layout = self.calculate_layout();
        unsafe {
            crate::macro_support::mut_array(
                self.get_unchecked_mut() as *mut _,
                layout.array_offset,
                layout.array_len,
            )
        }
    }

    /// Initializes an [`Array<T>`] by copying from an existing slice. Returns [`None`] if the slice's length is longer
    /// than can fit in this array's `Len` type.
    ///
    /// # Examples
    ///
    /// ```
    /// # use varlen::{VBox, Array};
    /// // Length fits in u8:
    /// let a: VBox<Array<u8, u8>> = VBox::new(Array::try_copy_from_slice(&[1, 2, 3]).unwrap());
    /// assert_eq!(&a[..], &[1, 2, 3]);
    /// // Length doesn't fit in u8:
    /// assert!(Array::<u8, u8>::try_copy_from_slice(&[1; 257]).is_none()); // Length is too large for u8.
    /// ```
    ///
    /// # See also
    ///
    /// When `Len=usize`, you may prefer [`Array::copy_from_slice`] which is guaranteed not to fail.
    pub fn try_copy_from_slice(src: &[T]) -> Option<SizedInit<crate::array_init::CopyFrom<T>, Len>>
    where
        T: Copy,
    {
        let len = Len::from_usize(src.len())?;
        Some(SizedInit(len, crate::array_init::CopyFrom(src)))
    }

    /// Initializes an [`Array<T>`] by cloning from an existing slice. Returns [`None`] if the slice's length is longer
    /// than can fit in this array's `Len` type.
    ///
    /// # Examples
    ///
    /// ```
    /// # use varlen::{VBox, Array};
    /// // Length fits in u8:
    /// let a: VBox<Array<Box<u8>, u8>> = VBox::new(Array::try_clone_from_slice(&[Box::new(1), Box::new(2)]).unwrap());
    /// assert_eq!(&a[..], &[Box::new(1), Box::new(2)]);
    /// // Length doesn't fit in u8:
    /// assert!(Array::<u8, u8>::try_clone_from_slice(&[1; 257]).is_none());
    /// ```
    ///
    /// # See also
    ///
    /// When `Len=usize`, you may prefer [`Array::copy_from_slice`] which is guaranteed not to fail.
    pub fn try_clone_from_slice(src: &[T]) -> Option<SizedInit<crate::array_init::CloneFrom<T>, Len>>
    where
        T: Clone,
    {
        let len = Len::from_usize(src.len())?;
        Some(SizedInit(len, crate::array_init::CloneFrom(src)))
    }
}

/// Initializer for an [`Array<T>`] given a specified length and array initializer.
///
/// # Examples
///
/// ```
/// # use varlen::{Array, VBox};
/// use varlen::array::SizedInit;
/// use varlen::array_init::FillSequentially;
/// let a: VBox<Array<u8>> = VBox::new(SizedInit(4usize, FillSequentially(|i| (i as u8) * 2)));
/// assert_eq!(&a[..], [0, 2, 4, 6]);
/// ```
///
/// # See also
///
/// Module [`crate::array_init`] has initializers that can be passed to `SizedInit`.
pub struct SizedInit<Init, Len: ArrayLen = usize>(pub Len, pub Init);

unsafe impl<T, Len: ArrayLen, Init: ArrayInitializer<T>> Initializer<Array<T, Len>>
    for SizedInit<Init, Len>
{
    #[inline(always)]
    fn calculate_layout_cautious(&self) -> Option<ArrayLayout> {
        let offset = core::mem::size_of::<Array<T, Len>>();
        let (array_offset, array_len, size) =
            crate::macro_support::cat_array_field_cautious::<T>(self.0.as_usize(), offset)?;
        Some(ArrayLayout {
            array_offset,
            array_len,
            size,
        })
    }

    unsafe fn initialize(self, dst: std::ptr::NonNull<Array<T, Len>>, layout: ArrayLayout) {
        let header = Array {
            len: self.0,
            _array: crate::macro_support::init_array(
                self.1,
                dst.cast::<u8>(),
                layout.array_offset,
                layout.array_len,
            ),
        };
        core::ptr::write(dst.as_ptr(), header);
    }
}

/// Integer types that are valid lengths for [`Array<T>`].
///
/// Valid options are: `u8`, `u16`, `u32`, `u64`, `usize`, and nothing else.
///
/// # Examples:
///
/// ```
/// use varlen::array::ArrayLen;
/// assert_eq!(Some(4u8), u8::from_usize(4usize));
/// ```
pub trait ArrayLen: 'static + Copy + private::Sealed {
    /// Converts to a [`usize`] via an `as` (truncating) conversion.
    ///
    /// # Examples
    ///
    /// ```
    /// # use varlen::array::ArrayLen;
    /// assert_eq!(4usize, 4u8.as_usize());
    /// assert_eq!(u64::MAX as usize, u64::MAX.as_usize());
    /// ```
    fn as_usize(self) -> usize;

    /// Converts from a [`usize`], returning `None` if it doesn't fit in the destination.
    ///
    /// # Examples
    ///
    /// ```
    /// # use varlen::array::ArrayLen;
    /// assert_eq!(Some(4u8), ArrayLen::from_usize(4));
    /// assert_eq!(None, u8::from_usize(257));
    /// ```
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
impl private::Sealed for usize {}

macro_rules! impl_arraylen {
    ($t:ty) => {
        impl ArrayLen for $t {
            #[inline(always)]
            fn as_usize(self) -> usize {
                self as usize
            }

            #[inline(always)]
            fn from_usize(x: usize) -> Option<Self> {
                Self::try_from(x).ok()
            }
        }
        impl private::Sealed for $t {}
    };
}
impl_arraylen!(u8);
impl_arraylen!(u16);
impl_arraylen!(u32);
impl_arraylen!(u64);

mod private {
    pub trait Sealed {}
}
