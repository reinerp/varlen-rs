#![warn(missing_docs)]
#![warn(rustdoc::missing_doc_code_examples)]
#![doc = crate::make_svgbobdoc!(
    //! A string with inline storage. 
    //! 
    //! This consists of an integer `length` field, followed immediately by the utf8 string payload. 
    //! For example, the [`VBox<String>`](crate::VBox) representation of `"hello"` is:
    //! 
    //! ```svgbob
    //! "VBox"
    //! +----------+
    //! | ptr      |
    //! +----------+
    //!      |
    //!      |   Str
    //!      |   +-----------+-----------+
    //!      '-> | 3: usize  | ""hello"" |
    //!          +-----------+-----------+
    //! ```
    //!
    //! Once allocated, the string size may not be modified.
    //! 
    //! # Examples
    //! 
    //! ```
    //! # use varlen::{VBox, Str};
    //! # use crate::varlen::{Layout, VarLen};
    //! let s = VBox::new(Str::copy_from_str("hello"));
    //! assert_eq!(&s[..], "hello");
    //! // Layout is as specified above:
    //! assert_eq!(s.calculate_layout().size(), std::mem::size_of::<usize>() + 5)
    //! ```
)]

use crate::array::{Array, ArrayLen};
use crate::{varlen_newtype, Initializer};
use core::pin::Pin;

varlen_newtype! {
    #[repr(transparent)]
    #[doc = crate::make_svgbobdoc!(
    /// A string with inline storage.
    ///
    /// This consists of an integer `length` field, followed immediately by the utf8 string payload.
    /// For example, the [`VBox<String>`](crate::VBox) representation of `"hello"` is:
    ///
    /// ```svgbob
    /// "VBox"
    /// +----------+
    /// | ptr      |
    /// +----------+
    ///      |
    ///      |   Str
    ///      |   +-----------+------+------+------+------+------+
    ///      '-> | 3: usize  | "'h'"| "'e'"| "'l'"| "'l'"| "'o'"|
    ///          +-----------+------+------+------+------+------+
    /// ```
    ///
    /// Once allocated, the string size may not be modified.
    ///
    /// # Examples
    ///
    /// ```
    /// # use varlen::{VBox, Str};
    /// # use crate::varlen::{Layout, VarLen};
    /// let s = VBox::new(Str::copy_from_str("hello"));
    /// assert_eq!(&s[..], "hello");
    /// // Layout is as specified above:
    /// assert_eq!(s.calculate_layout().size(), std::mem::size_of::<usize>() + 5)
    /// ```
    ///
    /// # Smaller length field
    ///
    /// You may choose to store the length of the string in a smaller integer type than [`usize`],
    /// just like in [`Array<T>`](crate::Array). Construction will fail if the string is too long for the
    /// integer type to express:
    ///
    /// ```
    /// # use varlen::{VBox, Str};
    /// // Short string fits:
    /// let s: VBox<Str<u8>> = VBox::new(Str::try_copy_from_str("hello").unwrap());
    /// // Long string doesn't fit:
    /// assert!(Str::<u8>::try_copy_from_str(std::str::from_utf8(&[b'a'; 257]).unwrap()).is_none());
    /// ```
    )]
    pub struct Str<(Len: ArrayLen = usize)>(Array<u8, Len>);

    with signature: impl<(Len: ArrayLen)> Str<(Len)>;

    with init: struct StrInit<_>(_);
    with inner_ref: fn inner(&self) -> &_;
    with inner_mut: fn inner_mut(self: _) -> _;
}

impl<Len: ArrayLen> core::ops::Deref for Str<Len> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        let slice: &[u8] = &self.inner();
        unsafe {
            // Safety: we only allow initialization from valid utf8
            core::str::from_utf8_unchecked(slice)
        }
    }
}

#[allow(rustdoc::missing_doc_code_examples)]
impl<Len: ArrayLen> Str<Len> {
    /// Mutable access to the underlying string.
    ///
    /// # Examples
    ///
    /// ```
    /// # use varlen::{VBox, Str};
    /// let mut s = VBox::new(Str::copy_from_str("hello"));
    /// s.as_mut().mut_slice().make_ascii_uppercase();
    /// assert_eq!(&s[..], "HELLO");
    /// ```
    pub fn mut_slice(self: Pin<&mut Self>) -> &mut str {
        let slice = self.inner_mut().mut_slice();
        unsafe {
            // Safety: we only allow initialization from valid utf8
            core::str::from_utf8_unchecked_mut(slice)
        }
    }
}

#[allow(rustdoc::missing_doc_code_examples)]
impl Str {
    /// Initializes a [`Str`] by copying from an existing [`&str`].
    ///
    /// # Examples
    ///
    /// ```
    /// # use varlen::{VBox, Str};
    /// let s = VBox::new(Str::copy_from_str("hello"));
    /// assert_eq!(&s[..], "hello");
    /// ```
    pub fn copy_from_str(s: &str) -> impl Initializer<Self> + '_ {
        Str::try_copy_from_str(s).unwrap()
    }
}
#[allow(rustdoc::missing_doc_code_examples)]
impl<Len: ArrayLen> Str<Len> {
    /// Initializes a [`Str`] from an existing [`&str`], or returns `None` if it doesn't fit in the length field.
    ///
    /// ```
    /// # use varlen::{VBox, Str};
    /// // Short string fits:
    /// let s: VBox<Str<u8>> = VBox::new(Str::try_copy_from_str("hello").unwrap());
    /// // Long string doesn't fit:
    /// assert!(Str::<u8>::try_copy_from_str(std::str::from_utf8(&[b'a'; 257]).unwrap()).is_none());
    /// ```
    pub fn try_copy_from_str(s: &str) -> Option<impl Initializer<Self> + '_> {
        Some(StrInit(Array::try_copy_from_slice(s.as_bytes())?))
    }
}
