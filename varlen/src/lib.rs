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

#[cfg(test)]
mod test_type;

pub use varlen_macro::define_varlen;
pub use vbox::VBox;
pub use owned::Owned;
pub use seq::Seq;
use core::ptr::NonNull;

/// Trait describing variable-length types.
/// 
/// All variable-length types must have:
///  * a way to report their own size and alignment
///    * because of the variable-length
///  * 
pub unsafe trait VarLen {
    // Layout
    type Layout: Layout;
    fn calculate_layout(&self) -> Self::Layout;
    const ALIGN: usize;

    /// Drop support
    const NEEDS_DROP_TAIL: bool;
    // Safety requirements:
    //  * must be called at most once on self
    //  * must not access tail fields after this call
    //  * layout must have been returned by `calculate_layout()` on 
    //    this object or its initializer.
    unsafe fn drop_tail(self: core::pin::Pin<&mut Self>, layout: Self::Layout);
}

pub trait Layout {
    fn size(&self) -> usize;
}

// Implementations:
//    VarLen + (), ref = &T
//      InlineArray, ref = &[T]
//      InlineString, ref = &str
//    Array + usize, ref = &[T]
//    String + usize, ref = &str
//
// CallerData:
//  * can specify CallerData on fields, referencing header
//  * can declare structs to have CallerData, which must be
//    passed in by the caller
//  * CallerData for Array<T> is `(usize, T::CallerData)`
//    * this is flexible enough for 2D arrays, for example
//    * although much too complex!
//  * can I have CallerData = an arena?
//    * sure, why not?
//    * reference type generally pairs CallerData with the pointer
//    * naturally gives wide pointers
//    * tricky question: what is the lifetime of CallerData?
//      * might need Generic Associated Types... https://blog.rust-lang.org/2021/08/03/GATs-stabilization-push.html
//  * decision: no support for PartialVarLen for now. Continue to
//    special-case arrays, as before.
//    * avoids any Generic Associated Types / lifetimes issues
//    * avoids needing a new set of &[T] types which also propagate
//      CallerData
// pub unsafe trait PartialVarLen {
//     type Layout: Layout;
//     type CallerData: Copy;  // for [T] this is usize (the length)
//     type FullRef;  // for [T] this is &[T]
//     type FullMut;  // for [T] this is &mut [T].

//     fn calculate_layout(&self, caller_data: &CallerData) -> Self::Layout;
//     const ALIGN: usize;
// }

// pub trait VarLen: PartialVarLen<ExtraData = ()> {}

/// Trait implementor promises:
///  * casting dst to Pin<&mut T> after calling initialize yields a valid reference.
pub unsafe trait ArrayInitializer<T> {
    unsafe fn initialize(self, dst: NonNull<[T]>);
}

/// Trait implementor promises:
///  * 'layout' is correct for the writes in 'initialize'
///  * after 'initialize', the 'VarLen::layout' matches the 'SizedInitializer::layout'.
pub unsafe trait VarLenInitializer<T: VarLen> {
    fn calculate_layout(&self) -> Option<T::Layout>;
    // Safety:
    //  * must be called with layout from `calculate_layout`
    //  * `dst` must be writable, with size as specified by `self.calculate_layout().size()`
    unsafe fn initialize(self, dst: NonNull<T>, layout: T::Layout);
}

/// Marker type for a variable-length field of a `#[define_varlen]` struct.
/// 
/// Fields of this type in a struct mean that there is a trailing variable-length array
/// in this struct. By desing, nn object of type `VarLenField` cannot be directly 
/// constructed within safe code; instead, you must construct the struct around it,
/// using one of the safe variable-length-struct initialization methods such as 
/// `varlen::boxed::Box::new`.
pub struct VarLenField<T>(core::marker::PhantomPinned, core::marker::PhantomData<T>);

impl<T> VarLenField<T> {
    /// The only way to construct a `VarLenField`.
    /// 
    /// Safety: when using this to construct a variable-length type, you must also allocate
    /// and initialize the tail of the type.
    pub unsafe fn new_unchecked() -> Self {
        VarLenField(core::marker::PhantomPinned, core::marker::PhantomData)
    }
}

/// Marker type for a variable-length field of a `#[define_varlen]` struct.
/// 
/// Fields of this type in a struct mean that there is a trailing variable-length array
/// in this struct. By desing, nn object of type `VarLenField` cannot be directly 
/// constructed within safe code; instead, you must construct the struct around it,
/// using one of the safe variable-length-struct initialization methods such as 
/// `varlen::boxed::Box::new`.
pub struct VarLenArray<T>(core::marker::PhantomPinned, core::marker::PhantomData<[T]>);

impl<T> VarLenArray<T> {
    /// The only way to construct a `VarLenField`.
    /// 
    /// Safety: when using this to construct a variable-length type, you must also allocate
    /// and initialize the tail of the type.
    pub unsafe fn new_unchecked() -> Self {
        VarLenArray(core::marker::PhantomPinned, core::marker::PhantomData)
    }
}