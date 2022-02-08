use crate::Initializer;

use super::{Layout, VarLen};

use core::alloc;
use core::marker::PhantomData;
use core::pin::Pin;
use core::ptr::NonNull;

/// A cross between a `T` and a `Box<T>`, for variable-length types.
///
/// The type `Owned<T>` is a pointer to a variable-length `T` that is responsible for calling
/// destructors on `T` (and its tail), but is *not* responsible for freeing the underlying storage.
pub struct Owned<'storage, T: VarLen>(NonNull<T>, PhantomData<&'storage [u8]>);

impl<'storage, T: VarLen> Owned<'storage, T> {
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
    pub fn get_alloc_layout(&self) -> alloc::Layout {
        unsafe {
            alloc::Layout::from_size_align_unchecked(T::calculate_layout(&*self).size(), T::ALIGN)
        }
    }

    /// Forgets the obligation to drop this.
    pub fn leak(self) -> Pin<&'storage mut T> {
        let Owned(mut ptr, _) = self;
        unsafe { Pin::new_unchecked(ptr.as_mut()) }
    }

    #[cfg(feature = "bumpalo")]
    #[inline]
    pub fn new_in(init: impl Initializer<T>, bump: &'storage bumpalo::Bump) -> Self {
        let layout = init.calculate_layout_cautious().unwrap();
        let ptr = bump
            .alloc_layout(alloc::Layout::from_size_align(layout.size(), T::ALIGN).unwrap())
            .cast::<T>();
        unsafe {
            init.initialize(ptr, layout);
        }
        Owned(ptr, PhantomData)
    }
}

impl<T: VarLen> Drop for Owned<'_, T> {
    fn drop(&mut self) {
        unsafe {
            // Careful sequencing of drop:
            //  1. Read the layout.
            //  2. Drop the header. Needs to happen before dropping the tail, because there might
            //     be a custom Drop on the header that reads the tail.
            //  3. Drop the tail. Uses the layout we read in step 1.
            let layout = T::calculate_layout(&*self);
            // let drop_tail_fn = self.as_mut().prepare_drop_tail();
            let p = self.0.as_ptr();
            core::ptr::drop_in_place(p);
            T::drop_tail(self.as_mut(), layout);
        }
    }
}

impl<T: VarLen> core::ops::Deref for Owned<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        unsafe { self.0.as_ref() }
    }
}

unsafe impl<T: VarLen> Initializer<T> for Owned<'_, T> {
    unsafe fn initialize(self, dst: NonNull<T>, layout: T::Layout) {
        // Safety:
        //  * Owned has unique access to its pointer
        //  * dst is unique
        //  * dst size is guaranteed by the SizedInitializer call
        core::ptr::copy_nonoverlapping(self.0.as_ptr(), dst.as_ptr(), layout.size());
        core::mem::forget(self);
    }

    #[inline]
    fn calculate_layout_cautious(&self) -> Option<T::Layout> {
        Some(T::calculate_layout(&*self))
    }
}
