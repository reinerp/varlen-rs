/// Marker type for a variable-length field of a `#[define_varlen]` struct.
///
/// Fields of this type in a struct mean that there is a trailing variable-length array
/// in this struct. By desing, nn object of type `VarLenField` cannot be directly
/// constructed within safe code; instead, you must construct the struct around it,
/// using one of the safe variable-length-struct initialization methods such as
/// `varlen::boxed::Box::new`.
pub struct FieldMarker<T>(core::marker::PhantomPinned, core::marker::PhantomData<T>);

impl<T> FieldMarker<T> {
    /// The only way to construct a `VarLenField`.
    ///
    /// Safety: when using this to construct a variable-length type, you must also allocate
    /// and initialize the tail of the type.
    pub unsafe fn new_unchecked() -> Self {
        FieldMarker(core::marker::PhantomPinned, core::marker::PhantomData)
    }
}

/// Marker type for a variable-length field of a `#[define_varlen]` struct.
///
/// Fields of this type in a struct mean that there is a trailing variable-length array
/// in this struct. By desing, nn object of type `VarLenField` cannot be directly
/// constructed within safe code; instead, you must construct the struct around it,
/// using one of the safe variable-length-struct initialization methods such as
/// `varlen::boxed::Box::new`.
pub struct ArrayMarker<T>(core::marker::PhantomPinned, core::marker::PhantomData<[T]>);

impl<T> ArrayMarker<T> {
    /// The only way to construct a `VarLenField`.
    ///
    /// Safety: when using this to construct a variable-length type, you must also allocate
    /// and initialize the tail of the type.
    pub unsafe fn new_unchecked() -> Self {
        ArrayMarker(core::marker::PhantomPinned, core::marker::PhantomData)
    }
}
