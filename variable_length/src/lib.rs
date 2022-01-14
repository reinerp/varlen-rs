#[doc(hidden)]
pub mod macro_support;
pub mod init;
pub mod boxed;
// pub mod seq;

pub use variable_length_macro::define_varlen;
use core::ptr::NonNull;

pub trait DropTailFn<T: ?Sized> {
    unsafe fn drop_tail(self, obj: core::pin::Pin<&mut T>);
}

pub trait VarLen {
    fn size(&self) -> usize;

    const ALIGN: usize;

    const NEEDS_DROP_TAIL: bool;

    type DropTailFn: DropTailFn<Self>;
    fn prepare_drop_tail(&self) -> Self::DropTailFn;
}

/// Trait implementor promises:
///  * casting dst to Pin<&mut T> after calling initialize yields a valid reference.
pub unsafe trait Initializer<T: ?Sized> {
    unsafe fn initialize(self, dst: NonNull<T>);
}

/// Trait implementor promises:
///  * 'layout' is correct for the writes in 'initialize'
///  * after 'initialize', the 'VarLen::layout' matches the 'SizedInitializer::layout'.
pub unsafe trait SizedInitializer<T: ?Sized>: Initializer<T> {
    fn size(&self) -> Option<usize>;
}

// Things I want to support:
//  * allocate a Pin<Box<Foo>> (global allocator)
//    * actually not ok. Need a new varlen::Box type, which deallocates with the correct
//  * allocate a Pin<&mut Foo> (bumpalo)
//  * support other allocation strategies too, e.g. 32-bit allocation
//    * what's the extensibility story for that?


/// Marker type for a variable-length field of a `#[define_varlen]` struct.
/// 
/// Fields of this type in a struct mean that there is a trailing variable-length array
/// in this struct. By desing, nn object of type `VarLenField` cannot be directly 
/// constructed within safe code; instead, you must construct the struct around it,
/// using one of the safe variable-length-struct initialization methods such as 
/// `variable_length::boxed::Box::new`.
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