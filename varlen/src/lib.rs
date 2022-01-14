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
//!   #[header]
//!   first_len: u8,
//! 
//!   #[header]
//!   mask: u8,
//! 
//!   #[varlen]
//!   arr0: [u16; self.first_len as usize],
//! 
//!   #[varlen]
//!   arr1: [u32; self.mask.count_ones() as usize],
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
//! use varlen::boxed::Box as VBox;
//! use varlen::init::{FillWithDefault, FillSequentially};
//! use varlen::seq::Seq;
//! # use varlen::define_varlen;
//! # #[define_varlen]
//! # struct CoolVarLen {
//! #   #[header]
//! #   first_len: u8,
//! # 
//! #   #[header]
//! #   mask: u8,
//! # 
//! #   #[varlen]
//! #   arr0: [u16; self.first_len as usize],
//! # 
//! #   #[varlen]
//! #   arr1: [u32; self.mask.count_ones() as usize],
//! # }
//! # fn main() {
//! let b: VBox<CoolVarLen> = VBox::new(cool_var_len::Init{
//!   header: cool_var_len::Header{
//!     first_len: 2,
//!     mask: 0xff,
//!   },
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
pub mod boxed;
pub mod seq;

#[cfg(test)]
mod test_type;

pub use varlen_macro::define_varlen;
use core::ptr::NonNull;

/// Trait describing variable-length types.
/// 
/// All variable-length types must have:
///  * a way to report their own size and alignment
///    * because of the variable-length
///  * 
pub unsafe trait VarLen {
    // Layout
    fn size(&self) -> usize;
    const ALIGN: usize;

    /// Drop support
    const NEEDS_DROP_TAIL: bool;
    type DropTailFn: DropTailFn<Self>;
    fn prepare_drop_tail(&self) -> Self::DropTailFn;
}

pub trait DropTailFn<T: ?Sized> {
    unsafe fn drop_tail(self, obj: core::pin::Pin<&mut T>);
}

/// Trait implementor promises:
///  * casting dst to Pin<&mut T> after calling initialize yields a valid reference.
pub unsafe trait ArrayInitializer<T> {
    unsafe fn initialize(self, dst: NonNull<[T]>);
}

/// Trait implementor promises:
///  * 'layout' is correct for the writes in 'initialize'
///  * after 'initialize', the 'VarLen::layout' matches the 'SizedInitializer::layout'.
pub unsafe trait VarLenInitializer<T> {
    fn required_size(&self) -> Option<usize>;
    unsafe fn initialize(self, dst: NonNull<T>);
}

/// Marker type for a variable-length field of a `#[define_varlen]` struct.
/// 
/// Fields of this type in a struct mean that there is a trailing variable-length array
/// in this struct. By desing, nn object of type `VarLenField` cannot be directly 
/// constructed within safe code; instead, you must construct the struct around it,
/// using one of the safe variable-length-struct initialization methods such as 
/// `varlen::boxed::Box::new`.
pub struct VarLenField<T: ?Sized>(core::marker::PhantomPinned, core::marker::PhantomData<T>);

impl<T: ?Sized> VarLenField<T> {
    /// The only way to construct a `VarLenField`.
    /// 
    /// Safety: when using this to construct a variable-length type, you must also allocate
    /// and initialize the tail of the type.
    pub unsafe fn new_unchecked() -> Self {
        VarLenField(core::marker::PhantomPinned, core::marker::PhantomData)
    }
}