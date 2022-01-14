#[doc(hidden)]
pub mod macro_support;
pub mod init;
pub mod boxed;
pub use variable_length_macro::define_varlen;
use core::alloc::Layout;
use core::ptr::NonNull;

pub trait DropTailFn<T: ?Sized> {
    unsafe fn drop_tail(self, obj: core::pin::Pin<&mut T>);
}

pub trait VarLen {
    fn layout(&self) -> Layout;

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
    fn layout(&self) -> Option<Layout>;
}

// Things I want to support:
//  * allocate a Pin<Box<Foo>> (global allocator)
//    * actually not ok. Need a new varlen::Box type, which deallocates with the correct
//  * allocate a Pin<&mut Foo> (bumpalo)
//  * support other allocation strategies too, e.g. 32-bit allocation
//    * what's the extensibility story for that?