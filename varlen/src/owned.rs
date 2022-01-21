use crate::VarLenInitializer;

use super::{DropTailFn, VarLen};

use core::alloc::Layout;
use core::ptr::NonNull;
use core::pin::Pin;
use core::marker::PhantomData;

/// A cross between a `T` and a `Box<T>`, for variable-length types.
/// 
/// The type `Owned<T>` is a pointer to a variable-length `T` that is responsible for calling 
/// destructors on `T` (and its tail), but is *not* responsible for freeing the underlying storage.
pub struct Owned<'storage, T: VarLen>(NonNull<T>, PhantomData<&'storage [u8]>);

impl<'storage, T: VarLen> Owned<'storage,T> {
    /// Safety: tail must be valid. For example, this can have been produced by a `SizedInitializer` 
    /// call or similar, on a correctly allocated buffer.
    pub unsafe fn from_raw(raw: NonNull<T>) -> Self {
        Owned(raw, PhantomData)
    }

    /// Safety: must not be used to produce a `&mut T`.
    pub unsafe fn into_raw(self) -> NonNull<T> {
        let result = self.0;
        core::mem::forget(self);
        result
    }

    /// Gets a (pinned) mutable reference.
    pub fn as_mut(&mut self) -> Pin<&mut T> {
        unsafe { Pin::new_unchecked(self.0.as_mut()) }
    }

    /// Layout of the `T`.
    pub fn layout(&self) -> Layout {
        unsafe {
            Layout::from_size_align_unchecked(self.size(), T::ALIGN)
        }
    }
}

impl<T: VarLen> Drop for Owned<'_, T> {
    fn drop(&mut self) {
        unsafe {
            // Careful sequencing of drop:
            //  1. Read the header, before we drop it.
            //  2. Drop the header. Needs to happen before dropping the tail, because there might
            //     be a custom Drop on the header that reads the tail.
            //  3. Drop the tail. Uses the values we read from the header in step 1.
            let drop_tail_fn = self.as_mut().prepare_drop_tail();
            let p = self.0.as_ptr();
            core::ptr::drop_in_place(p);
            drop_tail_fn.drop_tail(self.as_mut());
        }
    }
}

impl<T: VarLen> core::ops::Deref for Owned<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        unsafe { self.0.as_ref() }
    }
}

unsafe impl<T: VarLen> VarLenInitializer<T> for Owned<'_, T> {
    unsafe fn initialize(self, dst: NonNull<T>) {
        // Safety:
        //  * Owned has unique access to its pointer
        //  * dst is unique
        //  * dst size is guaranteed by the SizedInitializer call
        core::ptr::copy_nonoverlapping(self.0.as_ptr(), dst.as_ptr(), self.size());
        core::mem::forget(self);
    }

    #[inline]
    fn required_size(&self) -> Option<usize> {
        Some(self.size())
    }
}