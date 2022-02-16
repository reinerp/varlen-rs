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
//! type MyTuple = Tup3<FixedLen<usize>, Str, Str>;
//! # fn example1() {
//! let my_tuple: VBox<MyTuple> = VBox::new(tup3::Init(
//!     FixedLen(16), Str::copy_from_str("hello"), Str::copy_from_str("world")));
//! # }
//! 
//! // Define a newtype wrapper for it:
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
//! #     FixedLen(16), Str::copy_from_str("hello"), Str::copy_from_str("world")));
//! let my_struct: VBox<MyStruct> = VBox::new(MyStructInit(my_tuple));
//! # }
//! 
//! // Put multiple objects in a sequence, with tightly packed memory layout:
//! // let sequence: Seq<MyStruct> = seq![my_struct];
//! // TODO: clone() implementation.
//! 
//! // Define a variable-length struct via a procedural macro (optional crate feature).
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
//! #     FixedLen(16), Str::copy_from_str("hello"), Str::copy_from_str("world")));
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
//! # Old
//! 
//! As conceived by this library, variable-length types consist of a fixed-length header
//! (the type `Self`), followed by a variable-length tail which follows `Self` in memory.
//! The header has sufficient information in it to recover the length of the tail.
//!
//! Pointers to variable-length types are thin (one word), rather than wide (two words).
//! The following table shows the pointer types for variable-length types:
//!
//! | Name                   | Fixed-length type `T` | Variable-length type `T`                       |
//! |------------------------|-----------------------|------------------------------------------------|
//! | Immutable reference    | `&T`                  | `&T`                                           |
//! | Mutable reference      | `&mut T`              | [`std::pin::Pin<&mut T>`]                      |
//! | Owning, non-allocated  | `T`                   | `varlen::owned::Owned<'storage, T>` (sort of)  |
//! | Owning, allocated      | [`Box<T>`]            | `varlen::boxed::Box<T>`                        |
//!
//! Variable-length types must implement `VarLen`, which reports their size (including the tail)
//! and also specifies how to drop the tail. The primary way to construct variable-length types
//! is by the `#[define_varlen]` macro, for example:
//!
//! ```
//! use varlen::define_varlen;
//! #[define_varlen]
//! struct CoolVarLen {
//!   #[controls_layout]
//!   first_len: u8,
//!
//!   #[controls_layout]
//!   mask: u8,
//!
//!   #[varlen_array]
//!   arr0: [u16; *first_len as usize],
//!
//!   #[varlen_array]
//!   arr1: [u32; mask.count_ones() as usize],
//! }
//!
//! # fn main() {}
//! ```
//!
//! This example defines a variable-length type with `first_len` and `mask` in the header, and
//! `arr0` and `arr1` in the tail.
//!
//! If `T` is a variable-length type, we require the invariant that any reference `&T` must have
//! a valid tail, whose size matches its header. In order to maintain this invariant, you are
//! restricted in what operations you can do on variable-length types. For automatically defined
//! variable length types, the following operations are not possible to express in safe code:
//!  1) Changing what sizes are reported by the header. To prevent this, these fields are only
//!     immutably accessible in safe code.
//!  2) Getting access to a `T`. Doing so would violate (1), e.g. by moving the header without
//!     also moving the tail. Safe code does not give you access to `T`; use `Box<T>` or
//!    `Owned<'storage, T>` instead.
//!     2a) `Clone` and `Copy` are never implemented on `T`. They may be implemented on `Owned<T>`
//!     or `Box<T>` though.
//!  3) Getting access to a `&mut T`. Doing so would violate (1), e.g. by allowing
//!     `std::mem::swap`. Safe code only gives you access to `Pin<&mut T>`.
//!
//! Because you cannot directly get a `T`, functions like `varlen::Box::new()` or
//! `Seq::push()` take a different approach than the standard library's `Box::new()` or
//! `Vec::push()`. Instead of passing the already-constructed object `T`, we pass an "initializer"
//! object which knows how to write a `T` to a pointer. Initializers typically consist of the
//! actual value of the header, plus a lambda describing how to populate the tail. The following
//! example shows constructing a `Box<CoolVarLen>` by an initializer. Additionally, `Box` is itself
//! an initializer, so the example also shows using `Box` to push onto a `Seq`.
//!
//! ```
//! use varlen::vbox::VBox;
//! use varlen::array_init::{FillWithDefault, FillSequentially};
//! use varlen::seq::Seq;
//! # use varlen::define_varlen;
//! # #[define_varlen]
//! # struct CoolVarLen {
//! #   #[controls_layout]
//! #   first_len: u8,
//! #
//! #   #[controls_layout]
//! #   mask: u8,
//! #
//! #   #[varlen_array]
//! #   arr0: [u16; *first_len as usize],
//! #
//! #   #[varlen_array]
//! #   arr1: [u32; mask.count_ones() as usize],
//! # }
//! # fn main() {
//! let b: VBox<CoolVarLen> = VBox::new(cool_var_len::Init{
//!   first_len: 2,
//!   mask: 0xff,
//!   arr0: FillWithDefault,
//!   arr1: FillSequentially(|i| (i as u32) * 3),
//! });
//! let mut s: Seq<CoolVarLen> = Seq::new();
//! s.push(b);
//! # }
//! ```
)]

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
pub unsafe trait VarLen {
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
