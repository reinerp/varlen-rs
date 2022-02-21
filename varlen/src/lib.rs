#![doc = crate::doc_macro::make_svgbobdoc!(
//! # `varlen`
//! Ergonomic variable-length types.
//!
//! 1. [Summary](#summary)
//! 1. [Motivation](#motivation)
//! 1. [Examples](#examples)
//! 1. [Overview of types](#overview-of-types)
//! 1. [Use of Pin](#use-of-pin)
//! 1. [Feature flags](#feature-flags)
//! 
//! # Summary
//! 
//! `varlen` defines foundational types and traits for working with variable-length types in Rust.
//! 
//! The main example of variable-length type is a struct that stores a dynamically-sized array
//! directly in its storage, without requiring a pointer to a separate memory allocation. `varlen`
//! helps you define such types, and lets you build arbitrary concatenations and structs of them.
//! Additionally, it provides equivalents of the standard library's `Box<T>` and `Vec<T>` types
//! that are adapted to work well with variable-length types.
//! 
//! If you want to reduce the number of pointer indirections in your types by storing 
//! variable-sized arrays directly in your objects, then `varlen` is the library for you.
//! 
//! # Motivation
//! 
//! Traditionally when we use variable-sized data such as strings or arrays, we use a pointer
//! to a separately allocated object. For example, the following object ...
//! 
//! ```
//! type Person = (/* age */ usize, /* name */ Box<str>, /* email */ Box<str>);
//! let person: Person = 
//!     (16, Box::from("Harry Potter"), Box::from("harry.potter@example.com"));
//! ```
//! 
//! ... is represented in memory like this, with three separately allocated objects:
//! 
//! ```svgbob
//! "Person"
//! +------------+--------------------+-------------------+--------------------+-------------------+
//! | "16 (age)" | "0x1234 (str ptr)" | "12 (str length)" | "0x5678 (str ptr)" | "24 (str length)" |
//! +------------+--------------------+-------------------+--------------------+-------------------+
//!                 |                                       |
//!                 |                                       |
//!     .-----------'                         .-------------'
//!     |                                     |
//!     v "str payload"                       v "str payload"
//!     +------------------+                  +------------------------------+
//!     | "'Harry Potter'" |                  | "'harry.potter@example.com'" |
//!     +------------------+                  +------------------------------+
//! ```
//! 
//! Sometimes we can reduce the number of object allocations by bringing the variable-length
//! storage directly into the parent object, perhaps with a memory layout like this:
//! 
//! ```svgbob
//! +-----+
//! | ptr |
//! +-----+
//!   | 
//!   |
//!   |  "Person"
//!   |  +-------------+-------------------+------------------+-------------------+------------------------------+
//!   '->| "16 (age)"  | "12 (str length)" | "'Harry Potter'" | "24 (str length)" | "'harry.potter@example.com'" |
//!      +-------------+-------------------+------------------+-------------------+------------------------------+
//! ```
//! 
//! This layout reduced the number of object allocations from 3 to 2, potentially improving
//! memory allocator performance, and potentially also improving [CPU cache locality](https://en.wikipedia.org/wiki/Locality_of_reference).
//! It also reduced the number of pointers from 3 to 2, saving memory.
//! 
//! The main disadvantage of this layout is that size and layout of the `Person` object is not
//! known at compile time; it is only known at runtime, when the lengths of the strings are known.
//! Working with such layouts in plain Rust is cumbersome, and also requires unsafe code to do
//! the necessary pointer arithmetic.
//! 
//! `varlen` lets you easily define and use such types, without you having to write any
//! unsafe code. The following code will create an object with the memory layout from above:
//! 
//! ```
//! use varlen::prelude::*;
//! type Person = Tup3</* age */ FixedLen<usize>, /* name */ Str, /* email */ Str>;
//! let person: VBox<Person> = VBox::new(tup3::Init(
//!     FixedLen(16),
//!     Str::copy_from_str("Harry Potter"),
//!     Str::copy_from_str("harry.potter@example.com"),
//! ));
//! ```
//! 
//! # Examples
//! 
//! ```
//! use varlen::prelude::*;
//! 
//! // Define a variable-length tuple:
//! type MyTuple = Tup3<FixedLen<usize>, Str, Array<u16>>;
//! # fn example1() {
//! let my_tuple: VBox<MyTuple> = VBox::new(tup3::Init(
//!     FixedLen(16), Str::copy_from_str("hello"), Array::copy_from_slice(&[1u16, 2])));
//! 
//! // Put multiple objects in a sequence, with tightly packed memory layout:
//! let sequence: Seq<MyTuple> = seq![my_tuple.vcopy(), my_tuple.vcopy()];
//! 
//! # #[cfg(feature = "bumpalo")]
//! # {
//! // Or arena-allocate them, if the "bumpalo" crate feature is enabled:
//! let arena = bumpalo::Bump::new();
//! let arena_tuple: Owned<MyTuple> = Owned::new_in(my_tuple.vcopy(), &arena);
//! # }
//! # }
//! 
//! // Define a newtype wrapper for the tuple:
//! define_varlen_newtype! {
//!     #[repr(transparent)]
//!     pub struct MyStruct(MyTuple);
//! 
//!     with init: struct MyStructInit<_>(_);
//!     with inner_ref: fn inner(&self) -> &_;
//!     with inner_mut: fn inner_mut(self: _) -> _;
//! }
//! # fn example2() {
//! # let my_tuple: VBox<MyTuple> = VBox::new(tup3::Init(
//! #     FixedLen(16), Str::copy_from_str("hello"), Array::copy_from_slice(&[1u16, 2])));
//! let my_struct: VBox<MyStruct> = VBox::new(MyStructInit(my_tuple));
//! # }
//! 
//! // Define a variable-length struct via a procedural macro, if the "macro"
//! // crate feature is enabled.
//! # #[cfg(feature = "macro")]
//! # mod using_macro {
//! # use super::*;
//! #[define_varlen]
//! struct MyMacroStruct {
//!     age: usize,
//!     #[varlen]
//!     name: Str,
//!     #[varlen]
//!     email: Str,
//!     #[varlen]
//!     child: MyStruct,
//! }
//! # pub fn example3() {
//! # let my_tuple: VBox<MyTuple> = VBox::new(tup3::Init(
//! #     FixedLen(16), Str::copy_from_str("hello"), Array::copy_from_slice(&[1u16, 2])));
//! # let my_struct: VBox<MyStruct> = VBox::new(MyStructInit(my_tuple));
//! let s: VBox<MyMacroStruct> = VBox::new(
//!     my_macro_struct::Init{
//!         age: 16,
//!         name: Str::copy_from_str("Harry Potter"),
//!         email: Str::copy_from_str("harry.potter@example.com"),
//!         child: my_struct,
//!     }
//! );
//! # }
//! 
//! // #[define_varlen] also let you directly specify array lengths:
//! #[define_varlen]
//! struct MultipleArrays {
//!     #[controls_layout]
//!     len: usize,
//! 
//!     #[varlen_array]
//!     array1: [u16; *len],
//! 
//!     #[varlen_array]
//!     array2: [u8; *len],
//! 
//!     #[varlen_array]
//!     half_array: [u16; (*len) / 2],
//! }
//! # pub fn example4() {
//! let base_array = vec![0u16, 64000, 13, 105];
//! let a: VBox<MultipleArrays> = VBox::new(multiple_arrays::Init{
//!     len: base_array.len(),
//!     array1: FillSequentially(|i| base_array[i]),
//!     array2: FillSequentially(|i| base_array[base_array.len() - 1 - i] as u8),
//!     half_array: FillSequentially(|i| base_array[i * 2]),
//! });
//! # }
//! # }
//! 
//! # fn main() {
//! #   example1();
//! #   example2();
//! #   #[cfg(feature = "macro")] {
//! #     using_macro::example3();
//! #     using_macro::example4();
//! #   }
//! # }
//! ```
//! 
)]
//! # Overview of types
//!
//! `varlen` provides variable-length versions of various standard-library types and traits.
//! This table gives the correspondence:
//!
//!
//! | Name                     | Fixed-length type `T` | Variable-length type `T`                      | Notes                                                                                   |
//! |--------------------------|-----------------------|-----------------------------------------------|-----------------------------------------------------------------------------------------|
//! | Immutable reference      | `&T`                  | `&T`                                          |                                                                                         |
//! | Mutable reference        | `&mut T`              | [`Pin<&mut T>`][std::pin::Pin]                | `Pin<>` required for safety, see below                                                  |
//! | Owning, non-allocated    | `T`                   | [`Owned<'storage, T>`][crate::owned::Owned]   | `Owned<T>` is still a pointer to `T`'s payload                                          |
//! | Owning, allocated        | [`Box<T>`]            | [`VBox<T>`][crate::vbox::VBox]                |                                                                                         |
//! | Sequence                 | [`Vec<T>`]            | [`Seq<T>`][crate::seq::Seq]                   | `Seq` has tightly-packed _variable-size_ elements. Random access is somewhat restricted |
//! | String                   | [`String`]            | [`Str`][crate::str::Str]                      | String payload immediately follows the size, no pointer following                       |
//! | Array (fixed-size elems) | [`Box<[u16]>`]        | [`Array<u16>`][crate::array::Array]           | Array payload immediately follows the size, no pointer following                        |
//! | Tuple                    | `(T, U)`              | [`Tup2<T, U>`][crate::tuple::Tup2]            | Field `U` might not be at a statically known offset from start of object                |
//! | Clone                    | `Clone::clone()`      | [`VClone::vclone()`][crate::VClone::vclone]   |                                                                                         |
//! | Copy                     | <implicit>            | [`VCopy::vcopy()`][crate::VCopy::vcopy]       |                                                                                         |
//!
//!
//! # Use of `Pin`
//!
//! Mutable references to variable-length types use [`Pin<&mut T>`][std::pin::Pin] rather than
//! `&mut T`. By doing so, we prevent patterns such as calling [`std::mem::swap`] on
//! variable-length types. Such patterns would be a safety hazard, because the part of the type
//! that the Rust compiler knows about when calling [`std::mem::swap`] is just the
//! "fixed-size head" of the type. However, almost all variable-length types additionally have a
//! "variable-sized tail" that the Rust compiler doesn't know about. Swapping the head but not
//! the tail could violate a type's invariants, potentially breaking safety.
//!
//! If you never write `unsafe` code, you don't need to worry about this issue. The only practical
//! consequence is that mutable access to a variable-length type is always mediated through
//! [`Pin<&mut T>`][std::pin::Pin] rather than `&mut T`, and you will have to work with the slightly
//! more cumbersome `Pin` APIs.
//!
//! On the other hand, if you write `unsafe` code, you may have to be aware of the following
//! invariant. If `T` is a variable-length type, we require that any reference `&T` points to
//! a "valid" `T`, which we consider to be one which has a fixed-length head
//! (of size `std::mem::size_of::<T>()`) followed by a variable-length tail, and the head and
//! tail are "consistent" with each other. Here, "consistent" means that they were produced by
//! a call to one of the type's [`Initializer`] instances. In `unsafe` code, where you might
//! have access to a `&mut T` (without a `Pin`), you must avoid code patterns which modify the
//! head without also correspondingly modifying the tail.
//!
//! # Feature flags
//!
//! This crate has no *required* dependencies. The following feature flags exist, which can turn
//! on some dependencies.
//!
//! * `bumpalo`. Enables support for allocating an [`Owned<T>`][crate::owned::Owned] in an `bumpalo::Bump` arena. Adds a dependency on `bumpalo`.
//! * `macro`. Enables procedural macro support, for defining variable-length structs using [`#[define_varlen]`][crate::define_varlen]. Adds a dependency on `varlen_macro`, `syn` and `quote`.
//!

#![no_std]
extern crate alloc;

#[cfg(doc)]
extern crate std;
#[cfg(doc)]
use std::boxed::Box;
#[cfg(doc)]
use std::string::String;
#[cfg(doc)]
use std::vec::Vec;

pub mod array;
pub mod array_init;
#[doc(hidden)]
pub mod macro_support;
pub mod marker;
pub mod newtype;
pub mod owned;
pub mod prelude;
pub mod seq;
pub mod str;
pub mod tuple;
pub mod vbox;

mod doc_macro;

pub use crate::str::Str;
pub use array::Array;
use core::marker::PhantomData;
use core::ptr::NonNull;
pub use owned::Owned;
pub use seq::Seq;
#[cfg(feature = "macro")]
pub use varlen_macro::define_varlen;
pub use vbox::VBox;

/// The fundamental trait for variable-length types.
///
/// This type is analogous to `Sized` for fixed-length types. Provides functionality required
/// for:
/// * moving an object: need to know its [layout][`VarLen::calculate_layout`] and
///   [alignment][`VarLen::ALIGN`].
/// * dropping an object: the Rust compiler's tracking of [`Drop`] is insufficient for
///   variable-length types, since we need to track the variable-length tail too. Instead,
///   we use [`vdrop`][`VarLen::vdrop`].
///
/// Most users of this trait are not expected to implement this trait or call its functions
/// directly. Instead, you will typically use an existing implementation, such as a
/// [tuple type][`crate::tuple`] or a [`#[define_varlen]`][`crate::define_varlen`] struct.
///
/// # Examples
///
/// Generic function on variable-length type:
///
/// ```
/// use varlen::prelude::*;
/// fn push2<T: VarLen>(x: VBox<T>, y: VBox<T>, seq: &mut Seq<T>) {
///     seq.push(x);
///     seq.push(y);
/// }
/// let mut s = Seq::new();
/// push2(
///     VBox::new(Str::copy_from_str("hello")),
///     VBox::new(Str::copy_from_str("world")),
///     &mut s);
/// let mut iter = s.iter();
/// assert_eq!(&iter.next().unwrap()[..], "hello");
/// assert_eq!(&iter.next().unwrap()[..], "world");
/// assert!(iter.next().is_none());
/// ```
///
/// # Safety
///
/// To safely implement this trait, you must guarantee that the reported layout of your type
/// is correct. Specifically:
/// * [`VarLen::ALIGN`] must be a power of 2.
/// * the alignment reported by [`VarLen::ALIGN`] is large enough to support the alignment
///   requirements of all the fields within this type. An implication of this is that this
///   alignment must be at least large as `std::mem::align_of::<Self>()`, which is the alignment
///   requirement of the fixed-length header.
/// * the layout reported by [`VarLen::calculate_layout()`] reports the correct size of
///   the object, including its variable-length tail
///
/// The layout of an existing object must not change its size.
pub unsafe trait VarLen: Sized {
    /// This type's internal dynamic calculations of where its tail fields are.
    ///
    /// All you can do with this type is get the overall `size()`, and pass the layout
    /// to `drop_tail()` or `Initializer::initialize()`.
    type Layout: Layout;

    /// Calculates the layout of the internal fields of this object.
    ///
    /// # Examples
    ///
    /// ```
    /// use varlen::prelude::*;
    /// use varlen::Layout;
    /// let s = VBox::new(Str::copy_from_str("hello"));
    /// assert_eq!(s.calculate_layout().size(), std::mem::size_of::<usize>() + 5);
    /// ```
    fn calculate_layout(&self) -> Self::Layout;

    /// Alignment of this type, including its fixed-length header.
    ///
    /// # Examples
    ///
    /// ```
    /// use varlen::prelude::*;
    /// type T = Array<u64, u16>;
    /// // Fixed-length header is just the u16 length field:
    /// assert_eq!(std::mem::align_of::<T>(), std::mem::align_of::<u16>());
    /// // Variable-length tail includes the u64 array elements too:
    /// assert_eq!(T::ALIGN, std::mem::align_of::<u64>());
    /// ```
    ///
    /// # Safety
    ///
    /// Implementors of this trait must guarantee:
    /// * `ALIGN` is a power of 2.
    /// * `ALIGN` is at least equal to `core::mem::align_of::<Self>()`.
    const ALIGN: usize;

    /// Drops `self`.
    ///
    /// Rust's automatic [`Drop`] logic only accounts for the "head" (fixed-length) part
    /// of a type, and isn't aware of the presence of a variable-length "tail". On `VarLen`
    /// types, you should use `vdrop` instead of [`std::mem::drop`].
    ///
    /// # See also
    ///
    /// Containers types such as [`VBox`][crate::vbox::VBox], [`Owned`][crate::owned::Owned],
    /// and [`Seq`][crate::seq::Seq] take care of dropping their contents. You typically do
    /// not need to call [`vdrop`][`VarLen::vdrop`] directly.
    ///
    /// # Examples
    ///
    /// Drop on a [`VBox`]'s memory allocation.
    ///
    /// ```
    /// use varlen::prelude::*;
    /// use varlen::Layout as _;
    /// use std::pin::Pin;
    /// use std::alloc::Layout;
    /// type Ty = Tup2<FixedLen<String>, Str>;
    /// let v: VBox<Ty> =
    ///     VBox::new(tup2::Init(
    ///         FixedLen("hello".to_string()),
    ///         Str::copy_from_str("world"),
    ///     ));
    /// unsafe {
    ///     let p = v.into_raw();
    ///     let pp: Pin<&mut Ty> = Pin::new_unchecked(&mut *p);
    ///     let layout = pp.calculate_layout();
    ///     let size = layout.size();
    ///     pp.vdrop(layout);
    ///     std::alloc::dealloc(
    ///         p as *mut u8,
    ///         Layout::from_size_align(size, Ty::ALIGN).unwrap());
    /// }
    /// ```
    ///
    /// # Safety
    ///
    /// The caller must ensure:
    ///  * `vdrop` is called at most once on `self`
    ///  * `layout` must have been returned by `calculate_layout()` on
    ///    this object or `calculate_layout_cautious()` on its initializer.
    unsafe fn vdrop(self: core::pin::Pin<&mut Self>, layout: Self::Layout);

    /// If false, [`vdrop()`][`VarLen::vdrop`] is a noop, and may be skipped without changing
    /// behavior.
    ///
    /// This is the equvialent of [`std::mem::needs_drop()`] that accounts for the tail of
    /// the type too, not just the head.
    const NEEDS_VDROP: bool;
}

/// Support for cloning variable-length types.
///
/// # Examples
///
/// ```
/// use varlen::prelude::*;
///
/// let s_box: VBox<Str> = VBox::new(Str::copy_from_str("hello"));
/// let s: &Str = &*s_box;
/// let seq: Seq<Str> = seq![s.vclone(), s.vclone(), s.vclone()];
/// let mut iter = seq.iter();
/// assert_eq!(&iter.next().unwrap()[..], "hello");
/// assert_eq!(&iter.next().unwrap()[..], "hello");
/// assert_eq!(&iter.next().unwrap()[..], "hello");
/// assert!(iter.next().is_none());
/// ```
pub trait VClone<'a>: VarLen {
    /// An initializer that can produce a clone of an object of type `Self`.
    ///
    /// # Examples
    ///
    /// ```
    /// use varlen::prelude::*;
    ///
    /// let s_box: VBox<Str> = VBox::new(Str::copy_from_str("hello"));
    /// let s: &Str = &*s_box;
    /// let seq: Seq<Str> = seq![s.vclone(), s.vclone(), s.vclone()];
    /// let mut iter = seq.iter();
    /// assert_eq!(&iter.next().unwrap()[..], "hello");
    /// assert_eq!(&iter.next().unwrap()[..], "hello");
    /// assert_eq!(&iter.next().unwrap()[..], "hello");
    /// assert!(iter.next().is_none());
    /// ```
    type Cloner: Initializer<Self>;

    /// Returns an initializer that will clone `self` when run.
    ///
    /// # Examples
    ///
    /// ```
    /// use varlen::prelude::*;
    ///
    /// let s_box: VBox<Str> = VBox::new(Str::copy_from_str("hello"));
    /// let s: &Str = &*s_box;
    /// let seq: Seq<Str> = seq![s.vclone(), s.vclone(), s.vclone()];
    /// let mut iter = seq.iter();
    /// assert_eq!(&iter.next().unwrap()[..], "hello");
    /// assert_eq!(&iter.next().unwrap()[..], "hello");
    /// assert_eq!(&iter.next().unwrap()[..], "hello");
    /// assert!(iter.next().is_none());
    /// ```
    fn vclone(&'a self) -> Self::Cloner;
}

/// Support for shallow byte-wise copy of varlen types.
///
/// Types that implement this trait can be copied by a bulk copy of the
/// byte buffer at which the object is stored.
///
/// # Examples
///
/// ```
/// use varlen::prelude::*;
///
/// let s1: VBox<Str> = VBox::new(Str::copy_from_str("hello"));
/// let s2: VBox<Str> = VBox::new(s1.vcopy());
/// ```
///
/// # Safety
///
/// Implementors of this trait must guarantee that this type is safe for
/// bytewise copy. Usually it is sufficient to guarantee that all fields
/// implement `VCopy`.
pub unsafe trait VCopy<'a>: VClone<'a> {
    /// Returns an initializer that does a bulk byte copy of `self`.
    ///
    /// # Examples
    ///
    /// ```
    /// use varlen::prelude::*;
    ///
    /// let s1: VBox<Str> = VBox::new(Str::copy_from_str("hello"));
    /// let s2: VBox<Str> = VBox::new(s1.vcopy());
    /// ```
    fn vcopy(&'a self) -> VCopier<'a, Self> {
        VCopier(self)
    }
}

/// An initializer that constructs a bytewise copy of `T`.
///
/// See [`VCopy::vcopy`].
///
/// # Examples
///
/// ```
/// use varlen::prelude::*;
///
/// let s1: VBox<Str> = VBox::new(Str::copy_from_str("hello"));
/// let s2: VBox<Str> = VBox::new(s1.vcopy());
/// ```
pub struct VCopier<'a, T>(&'a T);

// Safety: implementor of `VCopy` promises that initialization via memcpy
// is safe.
unsafe impl<'a, T: VCopy<'a>> Initializer<T> for VCopier<'a, T> {
    #[inline]
    fn calculate_layout_cautious(&self) -> Option<T::Layout> {
        Some(self.0.calculate_layout())
    }

    #[inline]
    unsafe fn initialize(self, dst: NonNull<T>, layout: T::Layout) {
        let size = layout.size();
        core::ptr::copy_nonoverlapping(
            self.0 as *const _ as *const u8,
            dst.as_ptr() as *mut u8,
            size,
        );
    }
}

/// A layout of a variable-length object.
pub trait Layout: Eq {
    /// The size of the object in bytes.
    fn size(&self) -> usize;
}

/// A type that knows how to construct a variable-length object.
///
/// Construction of a variable-length object proceeds as follows:
///
/// 1. The `Initializer` calculates how much memory is required for the object,
///    returns that information to the caller.
/// 2. The caller, such as [`VBox`][crate::vbox::VBox] or [`Seq`][crate::seq::Seq],
///    reserves that much memory for the object.
/// 3. The `Initializer` populates the reserved memory.
///
/// An `Initializer` is responsible for steps (1) and (3) of this protocol. Step (1)
/// is handled by [`calculate_layout_cautious`][`Initializer::calculate_layout_cautious`],
/// and step (3) is handled by [`initialize`][`Initializer::initialize`].
///
/// Directly defining or using an `Initializer` involves writing `unsafe` code.
/// You can typically avoid this `unsafe` code by using the standard container types
/// such as [`VBox`][crate::vbox::VBox] aod [`Seq`][crate::seq::Seq] and the standard
/// initializers that the tuple/struct/array/string types provide.
///
/// # Examples
///
/// Full lifecycle of initialization and destruction:
///
/// ```
/// use varlen::prelude::*;
/// use varlen::Layout as _;
/// use std::alloc::Layout;
/// use std::pin::Pin;
/// use std::ptr::NonNull;
///
/// // Make an initializer. This is a cheap pointer-only operation.
/// let init = Str::copy_from_str("hello world");
///
/// // Step 1: calculate memory requirement.
/// let layout = init.calculate_layout_cautious().unwrap();
/// let alloc_layout = Layout::from_size_align(layout.size(), Str::<usize>::ALIGN).unwrap();
/// let mut s: Pin<&mut Str> = unsafe {
///     // Step 2: allocate memory.
///     let mem = std::alloc::alloc(alloc_layout);
///     let mut mem = NonNull::new(mem as *mut Str).unwrap();
///     // Step 3: populate the memory.
///     init.initialize(mem, layout);
///     // Now use the initialized object
///     Pin::new_unchecked(mem.as_mut())
/// };
/// // Read its value
/// assert_eq!(&s[..], "hello world");
/// // Eventually delete it
/// unsafe {
///     let layout = s.calculate_layout();
///     let ptr = s.as_mut().get_unchecked_mut() as *mut _ as *mut u8;
///     s.vdrop(layout);
///     std::alloc::dealloc(ptr, alloc_layout);
/// }
/// ```
///
/// # Safety
///
/// An implementor of this trait is required to ensure:
///  * When `self.initialize(dst, layout)` is called with `layout=self.calculate_layout_cautious()`,
///    the `initialize()` function doesn't write past offset `layout.size()` of `dst`.
///  * When `initialize(dst, layout)` is called with layout=self.calculate_layout_cautious()`, then
///    after the `initialize()` function returns the object `T` is "initialized", i.e. it is safe
///    to use as a `&T` or as a `Pin<&mut T>`.
///  * The layout returned by `calculate_layout_cautious()` matches the layout returned by
///    [`VarLen::calculate_layout()`] on the initialized object.
pub unsafe trait Initializer<T: VarLen> {
    /// Calculates the layout of the object, returning `None` if any of the calculated sizes
    /// or offsets would overflow `usize`.
    ///
    /// # Examples
    ///
    /// ```
    /// use varlen::prelude::*;
    /// use varlen::Layout as _;
    ///
    /// // Succeeds on most sizes:
    /// let init = SizedInit(4usize, FillWithDefault);
    /// assert_eq!(
    ///     Initializer::<Array<u32>>::calculate_layout_cautious(&init)
    ///     .unwrap().size(),
    ///     std::mem::size_of::<usize>() + 4 * std::mem::size_of::<u32>());
    /// // Fails on `usize` overflow:
    /// let init = SizedInit(usize::MAX, FillWithDefault);
    /// assert!(
    ///    Initializer::<Array<u32>>::calculate_layout_cautious(&init)
    ///    .is_none());
    /// ```
    fn calculate_layout_cautious(&self) -> Option<T::Layout>;

    /// Populates the destination pointer.
    ///
    /// See the [trait documentation][`Initializer`] for details on the initialization protocol.
    ///
    /// # Safety
    ///
    /// Trait implementor requirements are documented in the [trait documentation][`Initializer`].
    ///
    /// Additionally, the function caller must guarantee:
    ///
    /// * You must call `initialize` with the layout returned by
    ///   [`calculate_layout_cautious()`][`Initializer::calculate_layout_cautious`]
    ///   on the same initializer object.
    /// * `dst` must be writable, with size as specified by `self.calculate_layout().size()`.
    #[allow(rustdoc::missing_doc_code_examples)]
    unsafe fn initialize(self, dst: NonNull<T>, layout: T::Layout);
}

/// Presents a fixed-length type `T` as a variable-length type.
///
/// Useful for creating tuples consisting of some variable-length types
/// and some fixed-length types.
///
/// # Examples
///
/// Put a fixed-length type in a variable-sized box:
///
/// ```
/// use varlen::prelude::*;
/// let b: VBox<FixedLen<u16>> = VBox::new(FixedLen(4));
/// assert_eq!(b.0, 4);
/// ```
///
/// Create a tuple of variable-length types and a fixed-length type:
///
/// ```
/// use varlen::prelude::*;
/// let b: VBox<Tup2<FixedLen<u16>, Str>> = VBox::new(tup2::Init(
///     FixedLen(4), Str::copy_from_str("hello")));
/// assert_eq!(b.refs().0.0, 4);
/// assert_eq!(&b.refs().1[..], "hello");
/// ```
pub struct FixedLen<T>(pub T);

/// The variable-length layout of a fixed-length type.
///
/// # Examples
///
/// ```
/// use varlen::prelude::*;
/// use varlen::Layout as _;
/// let layout = FixedLen(4u16).calculate_layout_cautious().unwrap();
/// assert_eq!(layout.size(), std::mem::size_of::<u16>());
/// ```
pub struct FixedLenLayout<T>(PhantomData<T>);

impl<T> PartialEq for FixedLenLayout<T> {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}
impl<T> Eq for FixedLenLayout<T> {}

impl<T> Layout for FixedLenLayout<T> {
    fn size(&self) -> usize {
        core::mem::size_of::<T>()
    }
}

unsafe impl<T> Initializer<FixedLen<T>> for FixedLen<T> {
    fn calculate_layout_cautious(&self) -> Option<FixedLenLayout<T>> {
        Some(FixedLenLayout(PhantomData))
    }

    unsafe fn initialize(self, dst: NonNull<FixedLen<T>>, _layout: FixedLenLayout<T>) {
        dst.as_ptr().write(self);
    }
}

unsafe impl<T> VarLen for FixedLen<T> {
    type Layout = FixedLenLayout<T>;

    fn calculate_layout(&self) -> Self::Layout {
        FixedLenLayout(PhantomData)
    }

    const ALIGN: usize = core::mem::align_of::<T>();

    const NEEDS_VDROP: bool = true;

    unsafe fn vdrop(self: core::pin::Pin<&mut Self>, _layout: Self::Layout) {
        // Safety: drop_in_place is called only once, because Self::vdrop() is
        // called only once.
        core::ptr::drop_in_place(self.get_unchecked_mut() as *mut Self)
    }
}

/// Initializer that clones from a [`FixedLen<T>`].
///
/// # Examples
///
/// ```
/// use varlen::prelude::*;
/// let orig = FixedLen(4u16);
/// let the_clone = VBox::new(orig.vclone());
/// assert_eq!(orig.0, the_clone.0);
/// ```
pub struct FixedLenCloner<'a, T>(&'a T);

impl<'a, T: 'a + Clone> VClone<'a> for FixedLen<T> {
    type Cloner = FixedLenCloner<'a, T>;
    fn vclone(&'a self) -> Self::Cloner {
        FixedLenCloner(&self.0)
    }
}

unsafe impl<'a, T: Clone> Initializer<FixedLen<T>> for FixedLenCloner<'a, T> {
    fn calculate_layout_cautious(&self) -> Option<FixedLenLayout<T>> {
        Some(FixedLenLayout(PhantomData))
    }

    unsafe fn initialize(self, dst: NonNull<FixedLen<T>>, _layout: FixedLenLayout<T>) {
        dst.as_ptr().write(FixedLen(self.0.clone()));
    }
}

// Safety: coyable if T is.
unsafe impl<'a, T: 'a + Copy> VCopy<'a> for FixedLen<T> {}
