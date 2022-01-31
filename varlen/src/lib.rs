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
//! use varlen::init::{FillWithDefault, FillSequentially};
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

#[doc(hidden)]
pub mod macro_support;
pub mod init;
pub mod owned;
pub mod vbox;
pub mod seq;
pub mod marker;
pub mod generic;
pub mod array;
pub mod str;
pub mod newtype;

#[cfg(test)]
mod test_type;

pub use varlen_macro::define_varlen;
pub use vbox::VBox;
pub use owned::Owned;
pub use seq::Seq;
pub use array::Array;
pub use crate::str::Str;
use core::ptr::NonNull;

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
pub trait Layout {
    /// The size of the object in bytes.
    fn size(&self) -> usize;
}

/// Trait implementor promises:
///  * casting dst to `&mut [T]` after calling initialize yields a valid reference.
pub unsafe trait ArrayInitializer<T> {
    /// Fills the slice.
    unsafe fn initialize(self, dst: NonNull<[T]>);
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

