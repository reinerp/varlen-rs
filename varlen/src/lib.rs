#![doc = crate::doc_macro::make_svgbobdoc!(
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
//!                 |                                     |
//!                 |                                     |
//!     .-----------'                         .-----------'
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
//! # fn example3() {
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
//! # fn example4() {
//! let base_array = vec![0u16, 64000, 13, 105];
//! let a: VBox<MultipleArrays> = VBox::new(multiple_arrays::Init{
//!     len: base_array.len(),
//!     array1: FillSequentially(|i| base_array[i]),
//!     array2: FillSequentially(|i| base_array[base_array.len() - 1 - i] as u8),
//!     half_array: FillSequentially(|i| base_array[i * 2]),
//! });
//! # }
//! 
//! # fn main() {
//! #   example1();
//! #   example2();
//! #   example3();
//! #   example4();
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
//! * `macro`. Enables procedural macro support, for defining variable-length structs using [`#[define_varlen]`][crate::define_varlen]. Adds a dependency on `syn` and `quote`.
//!

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
pub use varlen_macro::define_varlen;
pub use vbox::VBox;

/// Trait describing variable-length types.
///
/// All variable-length types must have:
///  * a way to report their own size and alignment
///    * because of the variable-length
///  *
pub unsafe trait VarLen: Sized {
    /// This type's internal dynamic calculations of where its tail fields are.
    ///
    /// All you can do with this type is get the overall `size()`, and pass the layout
    /// to `drop_tail()` or `Initializer::initialize()`.
    type Layout: Layout;

    /// Calculates the layout of the internal fields of this object.
    fn calculate_layout(&self) -> Self::Layout;

    /// Alignment of this type.
    ///
    /// To be safe, trait implementor must guarantee:
    /// * `ALIGN` is a power of 2.
    /// * `ALIGN` is at least equal to `core::mem::align_of::<Self>()`.
    const ALIGN: usize;

    /// If true, `drop_tail()` is a noop.
    const NEEDS_DROP_TAIL: bool;

    /// Drops the tail of `self`. The "tail" is the part of the type that Rust's automatic
    /// `Drop` logic _doesn't_ take care of.
    ///
    /// Safety requirements:
    ///  * must be called at most once on `self`
    ///  * caller must not access tail fields after this call
    ///  * layout must have been returned by `calculate_layout()` on
    ///    this object or `calculate_layout_cautious()` on its initializer.
    unsafe fn drop_tail(self: core::pin::Pin<&mut Self>, layout: Self::Layout);
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
    type Cloner: Initializer<Self>;
    fn vclone(&'a self) -> Self::Cloner;
}

/// Safety: implementor promises that all fields implement VCopy.
pub unsafe trait VCopy<'a>: VClone<'a> {
    fn vcopy(&'a self) -> VCopier<'a, Self> {
        VCopier(self)
    }
}

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

// pub trait VClone: VarLen where for<'a> CloneOf<'a, Self>: Initializer<Self> {}
// pub trait VCopy: VarLen where for<'a> CopyOf<'a, Self>: Initializer<Self> {}

// pub struct CloneOf<'a, T>(pub &'a T);
// pub struct CopyOf<'a, T>(pub &'a T);

/// A layout of a variable-length object.
pub trait Layout: Eq {
    /// The size of the object in bytes.
    fn size(&self) -> usize;
}

/// Trait implementor promises:
///  * 'layout' is correct for the writes in 'initialize'
///  * after 'initialize', the 'VarLen::layout' matches the 'SizedInitializer::layout'.
pub unsafe trait Initializer<T: VarLen> {
    fn calculate_layout_cautious(&self) -> Option<T::Layout>;
    // Safety:
    //  * must be called with layout from `calculate_layout`
    //  * `dst` must be writable, with size as specified by `self.calculate_layout().size()`
    unsafe fn initialize(self, dst: NonNull<T>, layout: T::Layout);
}

/// Presents a fixed-length type `T` as a variable-length type.
///
/// Useful for creating tuples consisting of some variable-length types
/// and some fixed-length types.
///
/// # Examples
///
/// ```
/// use varlen::prelude::*;
/// let b: VBox<FixedLen<u16>> = VBox::new(FixedLen(4));
/// assert_eq!(b.0, 4);
/// ```
pub struct FixedLen<T>(pub T);

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

    const NEEDS_DROP_TAIL: bool = false;

    unsafe fn drop_tail(self: core::pin::Pin<&mut Self>, _layout: Self::Layout) {}
}

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
