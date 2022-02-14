/// Marker type for a variable-length field of a `#[define_varlen]` struct.
///
/// Fields of this type in a struct mean that there is a trailing variable-length array
/// in this struct. By design, an object of type `FieldMarker` cannot be directly
/// constructed within safe code; instead, you must construct the struct around it,
/// using one of the safe variable-length-struct initialization methods such as
/// [`crate::vbox::VBox::new`].
pub struct FieldMarker<T>(core::marker::PhantomPinned, core::marker::PhantomData<T>);

impl<T> FieldMarker<T> {
    /// The only way to construct a `FieldMarker`.
    ///
    /// Safety: when using this to construct a variable-length type, you must also allocate
    /// and initialize the tail of the type.
    pub unsafe fn new_unchecked() -> Self {
        FieldMarker(core::marker::PhantomPinned, core::marker::PhantomData)
    }
}

/// Marker type for a variable-length array field of a `#[define_varlen]` struct.
///
/// Fields of this type in a struct mean that there is a trailing variable-length array
/// in this struct. By design, an object of type `ArrayMarker` cannot be directly
/// constructed within safe code; instead, you must construct the struct around it,
/// using one of the safe variable-length-struct initialization methods such as
/// [`crate::vbox::VBox::new`].
pub struct ArrayMarker<T>(core::marker::PhantomPinned, core::marker::PhantomData<[T]>);

impl<T> ArrayMarker<T> {
    /// The only way to construct a `ArrayMarker`.
    ///
    /// Safety: when using this to construct a variable-length type, you must also allocate
    /// and initialize the tail of the type.
    pub unsafe fn new_unchecked() -> Self {
        ArrayMarker(core::marker::PhantomPinned, core::marker::PhantomData)
    }
}
