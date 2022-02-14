#![warn(missing_docs)]
#![allow(rustdoc::missing_doc_code_examples)]
//! Marker types for variable-length fields of structs.


/// Marker type for a variable-length field of a struct.
/// 
/// # Examples
/// 
/// Variable-length fields of `#[define_varlen]` structs are `FieldMarker`:
/// 
/// ```
/// use varlen::define_varlen;
/// use varlen::str::Str;
/// use varlen::marker::FieldMarker;
/// 
/// #[define_varlen]
/// struct S {
///     #[varlen]
///     x: Str,
/// }
/// 
/// fn field_x(s: &S) -> &FieldMarker<Str> {
///     &s.x
/// }
/// # fn main() { }
/// ```
/// 
/// # Usage
///
/// A `FieldMarker<T>` in a struct indicates that the struct has a variable-length object `T` in
/// its tail. The `FieldMarker<T>` itself takes no space in the struct; it is purely a "marker" type
/// on which to hang documentation of the struct.
/// 
/// This marker type also plays an important safety role, preventing you from creating a 
/// variable-length object without also creating a valid tail for it, unless you use unsafe code.
pub struct FieldMarker<T>(core::marker::PhantomPinned, core::marker::PhantomData<T>);

impl<T> FieldMarker<T> {
    /// The only way to construct a `FieldMarker`.
    /// 
    /// You typically should not use this directly, and should instead use one of the safe 
    /// initialization methods provided by the underlying struct, such as [`crate::vbox::VBox::new`].
    /// 
    /// # Safety requirements
    /// 
    /// The returned marker must be placed in a variable-length struct which has a valid tail
    /// for this field.
    #[allow(rustdoc::missing_doc_code_examples)]
    pub unsafe fn new_unchecked() -> Self {
        FieldMarker(core::marker::PhantomPinned, core::marker::PhantomData)
    }
}

/// Marker type for a variable-length array field of a struct.
/// 
/// # Examples
/// 
/// Variable-length array fields of `#[define_varlen]` structs are `ArrayMarker`:
/// 
/// ```
/// use varlen::define_varlen;
/// use varlen::str::Str;
/// 
/// #[define_varlen]
/// struct S {
///     #[controls_layout]
///     len: usize,
/// 
///     #[varlen_array]
///     array: [u16; *len],
/// }
/// 
/// fn field_array(s: &S) -> &ArrayMarker<u16> {
///     &s.x
/// }
/// ```
/// 
/// # Usage
///
/// An `ArrayMarker<T>` in a struct indicates that the struct has a variable-length array `[T]` in
/// its tail. The `ArrayMarker<T>` itself takes no space in the struct; it is purely a "marker" type
/// on which to hang documentation of the struct.
/// 
/// This marker type also plays an important safety role, preventing you from creating a 
/// variable-length object without also creating a valid tail for it, unless you use unsafe code.
pub struct ArrayMarker<T>(core::marker::PhantomPinned, core::marker::PhantomData<[T]>);

impl<T> ArrayMarker<T> {
    /// The only way to construct an `ArrayMarker`.
    /// 
    /// You typically should not use this directly, and should instead use one of the safe 
    /// initialization methods provided by the underlying struct, such as [`crate::vbox::VBox::new`].
    /// 
    /// # Safety requirements
    /// 
    /// The returned marker must be placed in a variable-length struct which has a valid tail
    /// for this field.
    #[allow(rustdoc::missing_doc_code_examples)]
    pub unsafe fn new_unchecked() -> Self {
        ArrayMarker(core::marker::PhantomPinned, core::marker::PhantomData)
    }
}
