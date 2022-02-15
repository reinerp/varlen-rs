#![warn(missing_docs)]
#![warn(rustdoc::missing_doc_code_examples)]

//! Macros for defining newtype wrappers around variable-length types.
//!
//! # Examples
//!
//! As an example, we'll create a wrapper around `Str` that also tracks the number of UTF8 code
//! points in the string. To ensure these stay in sync, we allow the users of our library to
//! access the codepoint count in immutable form (read access) but not mutable form (write access).
//! The newtype wrapper helps us ensure this visibility as desired:
//!
//! ```
//! use varlen::prelude::*;
//!
//! define_varlen_newtype! {
//!     #[repr(transparent)]
//!     /// A string which counts UTF8 code points.
//!     pub struct CountedStr(
//!         Tup2<
//!             // Count of UTF8 code points in the str
//!             FixedLen<usize>,
//!             // Paylod
//!             Str
//!         >);
//!
//!     with init: struct CountedStrInitImpl<_>(_);
//!     with inner_ref: fn inner(&self) -> &_;
//!     with inner_mut: fn inner_mut(self: _) -> _;
//! }
//!
//! impl CountedStr {
//!     pub fn from_str<'a>(src: &'a str) -> impl 'a + Initializer<Self> {
//!         let count = src.chars().count();
//!         CountedStrInitImpl(
//!             tup2::Init(
//!                 FixedLen(count),
//!                 Str::copy_from_str(src),
//!             )
//!         )
//!     }
//!
//!     pub fn char_count(&self) -> usize {
//!         self.inner().refs().0.0
//!     }
//!
//!     pub fn str(&self) -> &str {
//!         &self.inner().refs().1[..]
//!     }
//! }
//!
//! let s = VBox::new(CountedStr::from_str("hellö wörld"));
//! assert_eq!(11, s.char_count());
//! assert_eq!(13, s.str().len());
//! ```

/// Defines a newtype wrapper around a varlen type.
///
/// # Examples
///
/// As an example, we'll create a wrapper around `Str` that also tracks the number of UTF8 code
/// points in the string. To ensure these stay in sync, we allow the users of our library to
/// access the codepoint count in immutable form (read access) but not mutable form (write access).
/// The newtype wrapper helps us ensure this visibility as desired:
///
/// ```
/// use varlen::prelude::*;
///
/// define_varlen_newtype! {
///     #[repr(transparent)]
///     /// A string which counts UTF8 code points.
///     pub struct CountedStr(
///         Tup2<
///             // Count of UTF8 code points in the str
///             FixedLen<usize>,
///             // Paylod
///             Str
///         >);
///
///     with init: struct CountedStrInitImpl<_>(_);
///     with inner_ref: fn inner(&self) -> &_;
///     with inner_mut: fn inner_mut(self: _) -> _;
/// }
///
/// impl CountedStr {
///     pub fn from_str<'a>(src: &'a str) -> impl 'a + Initializer<Self> {
///         let count = src.chars().count();
///         CountedStrInitImpl(
///             tup2::Init(
///                 FixedLen(count),
///                 Str::copy_from_str(src),
///             )
///         )
///     }
///
///     pub fn char_count(&self) -> usize {
///         self.inner().refs().0.0
///     }
///
///     pub fn str(&self) -> &str {
///         &self.inner().refs().1[..]
///     }
/// }
///
/// let s = VBox::new(CountedStr::from_str("hellö wörld"));
/// assert_eq!(11, s.char_count());
/// assert_eq!(13, s.str().len());
/// ```
///
/// # Generated items
///
/// The macro generates the following items:
///
/// * The struct, exactly as you specified. This is `CountedStr` in the example above.
/// * An initializer struct, with signature as specified. The type argument is expected to be an
///   initializer for the inner type.
/// * Two member functions, with signatures as above, for accessing the inner type immutably
///   (`inner_ref`) and mutably (`inner_mut`).
/// * Implementations of traits [`crate::VarLen`] and [`crate::Initializer`] for your newtype.
///
/// For all of these except the trait implementations, you must provide a signature in the
/// macro invocation site. The signature can specify the name, visibility, and documentation
/// for this item.
///
/// # Using generics
///
/// Your type may use generics. However, the syntax diverges a little from standard Rust
/// syntax, because of limitations of Rust's `macro_rules`. The requirements are:
///
/// * Every generic must be surrounded by parentheses in the type's definition.
///
/// * You must provide an explicit `with signature` clause to define the `impl`'s signatures.
///
/// The following example shows this in action:
///
/// ```
/// use varlen::prelude::*;
///
/// define_varlen_newtype! {
///     #[repr(transparent)]
///     /// A string which counts UTF8 code points.
///     pub struct TwoArrays<(T: Copy), (U: Clone = u16)>(
///         pub Tup2<
///             Array<T>,
///             Array<U>,
///         >);
///
///     with signature: impl<(T: Copy), (U: Clone)> TwoArrays <(T), (U)> { _ }
///     with init: pub struct TwoArraysInit<_>(_);
///     with inner_ref: pub fn inner(&self) -> &_;
///     with inner_mut: pub fn inner_mut(self: _) -> _;
/// }
///
/// let t: VBox<TwoArrays<u16, u8>> = VBox::new(TwoArraysInit(
///     tup2::Init(
///         Array::copy_from_slice(&[1, 2, 3]),
///         Array::copy_from_slice(&[4, 5, 6, 7]),
///     )
/// ));
/// assert_eq!(&t.inner().refs().0[..], &[1, 2, 3]);
/// assert_eq!(&t.inner().refs().1[..], &[4, 5, 6, 7]);
/// ```
#[macro_export]
macro_rules! define_varlen_newtype {
    (
        #[repr(transparent)]
        $(#[$attrs:meta])*
        $tyvis:vis struct $outer:ident $(< $( ( $($generics:tt)* ) ),* >)? ($fieldvis:vis $inner:ty);

        $(
            with signature: impl< $( ( $($generic_params:tt)* ) ),* > $ignored:ident < $( ($($generics_apply:tt)*) ),* > { _ }
        )?

        with init:
            $(#[$initattrs:meta])*
            $initvis:vis struct $init:ident < _ > ($initfieldvis:vis _);
        with inner_ref:
            $(#[$refattrs:meta])*
            $refvis:vis fn $ref:ident(&self) -> &_;
        with inner_mut:
            $(#[$mutattrs:meta])*
            $mutvis:vis fn $mut:ident(self: _) -> _;

    ) => {
        #[repr(transparent)]
        $(#[$attrs])*
        $tyvis struct $outer $(< $($($generics)*),* >)* ($fieldvis $inner);

        $(#[$initattrs])*
        $initvis struct $init < InnerInit >($initfieldvis InnerInit);

        unsafe impl $(< $($($generic_params)*),* >)* $crate::VarLen for $outer $(< $($($generics_apply)*),* >)* {
            type Layout = <$inner as $crate::VarLen>::Layout;
            #[inline(always)]
            fn calculate_layout(&self) -> Self::Layout {
                self.0.calculate_layout()
            }

            const ALIGN: usize = <$inner as $crate::VarLen>::ALIGN;
            const NEEDS_DROP_TAIL: bool = <$inner as $crate::VarLen>::NEEDS_DROP_TAIL;

            #[inline(always)]
            unsafe fn drop_tail(self: ::core::pin::Pin<&mut Self>, layout: Self::Layout) {
                <$inner as $crate::VarLen>::drop_tail(self.map_unchecked_mut(|outer| &mut outer.0), layout);
            }
        }

        unsafe impl< $( $($($generic_params)*,)* )* InnerInit: $crate::Initializer<$inner>> $crate::Initializer<$outer $(< $($($generics_apply)*),* >)* > for $init<InnerInit> {
            #[inline(always)]
            fn calculate_layout_cautious(&self) -> ::core::option::Option<<$inner as $crate::VarLen>::Layout> {
                <InnerInit as $crate::Initializer<$inner>>::calculate_layout_cautious(&self.0)
            }

            #[inline(always)]
            unsafe fn initialize(self, dst: ::core::ptr::NonNull<$outer $(< $($($generics_apply)*),* >)* >, layout: <$inner as $crate::VarLen>::Layout) {
                self.0.initialize(dst.cast::<$inner>(), layout);
            }
        }

        #[allow(rustdoc::missing_doc_code_examples)]
        impl $(< $($($generic_params)*),* >)* $outer $(< $($($generics_apply)*),* >)* {
            $(#[$refattrs])*
            $refvis fn $ref(&self) -> & $inner {
                &self.0
            }

            $(#[$mutattrs])*
            $mutvis fn $mut(self: ::core::pin::Pin<&mut Self>) -> ::core::pin::Pin<&mut $inner> {
                unsafe {
                    // Safety:
                    // * inner is Unpin if and only if outer is Unpin.
                    // * we don't move out of outer in this lambda.
                    ::core::pin::Pin::map_unchecked_mut(self, |s| &mut s.0)
                }
            }
        }
    }
}
#[doc(inline)]
pub use define_varlen_newtype;

/// Lifts a [`crate::Initializer<T>`] implementation to a newtype.
///
/// # Examples
///
/// ```
/// use varlen::prelude::*;
///
/// /// Custom initializer for initializing from arrays of size 3.
/// pub struct Init3Array(SizedInit<MoveFrom<u16, 3>>);
///
/// impl Init3Array {
///     pub fn new(arr: [u16; 3]) -> Self {
///         Init3Array(SizedInit(3, MoveFrom(arr)))
///     }
/// }
///
/// impl_initializer_as_newtype! {
///     impl Initializer<Array<u16>> for Init3Array { _ }
/// }
///
/// let v: VBox<Array<u16>> = VBox::new(Init3Array::new([4, 5, 6]));
/// assert_eq!(&v[..], &[4, 5, 6]);
/// ```
///
/// # Generics
///
/// Your type may use generics. However, the syntax diverges a little from standard Rust
/// syntax, because of limitations of Rust's `macro_rules`. We require that the first set
/// of generics in the `impl` statement uses parens `(...)` around each generic argument.
///
/// The following example shows this in action:
///
/// ```
/// use varlen::prelude::*;
///
/// /// Custom initializer for cloning from arrays of size 3.
/// pub struct Init3Array<'a, T>(SizedInit<CloneFrom<'a, T>>);
///
/// impl<'a, T: Clone> Init3Array<'a, T> {
///     pub fn new(arr: &'a [T; 3]) -> Self {
///         Init3Array(SizedInit(3, CloneFrom(arr)))
///     }
/// }
///
/// impl_initializer_as_newtype! {
///     impl<('a), (T: Clone)> Initializer<Array<T>> for Init3Array<'a, T> { _ }
/// }
///
/// let v: VBox<Array<u64>> = VBox::new(Init3Array::new(&[4, 5, 6]));
/// assert_eq!(&v[..], &[4, 5, 6]);
/// ```
#[macro_export]
macro_rules! impl_initializer_as_newtype {
    (
        impl $(< $( ( $($generic_params:tt)* ) ),* >)* $(varlen::)? Initializer<$t:ty> for $init:ty { _ }
    ) => {
        unsafe impl $(< $($($generic_params)*,)* >)* $crate::Initializer<$t> for $init {
            #[inline(always)]
            fn calculate_layout_cautious(&self) -> ::core::option::Option<<$t as $crate::VarLen>::Layout> {
                $crate::Initializer::<$t>::calculate_layout_cautious(&self.0)
            }

            #[inline(always)]
            unsafe fn initialize(self, dst: ::core::ptr::NonNull<$t>, layout: <$t as $crate::VarLen>::Layout) {
                // Safety:
                // * validity of dst is ensured by caller
                // * layout matches what we called layout on above, on `self.0`
                self.0.initialize(dst, layout);
            }
        }
    }
}
#[doc(inline)]
pub use impl_initializer_as_newtype;
