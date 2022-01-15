use super::{DropTailFn, VarLenInitializer, VarLen};

use core::alloc::Layout;
use core::ptr::NonNull;
use core::pin::Pin;

/// [`Box<T>`], but for [`VarLen`] types T.
pub struct VBox<T: VarLen>(NonNull<T>);

#[inline(never)]
#[cold]
fn allocation_overflow() -> ! {
    panic!("Allocation size overflow")
}

impl<T: VarLen> VBox<T> {
    pub fn new(init: impl VarLenInitializer<T>) -> Self {
        let size = init.required_size().unwrap_or_else(|| allocation_overflow());
        let layout = Layout::from_size_align(size, T::ALIGN).unwrap_or_else(|_| allocation_overflow());
        unsafe {
            let p = std::alloc::alloc(layout) as *mut T;
            init.initialize(NonNull::new_unchecked(p));
            let mut p=  NonNull::new_unchecked(p);
            debug_assert_eq!(p.as_mut().size(), size);
            VBox(p)
        }
    }

    pub fn as_mut(&mut self) -> Pin<&mut T> {
        unsafe { Pin::new_unchecked(self.0.as_mut()) }
    }

    // Safety: must not be used to produce a `&mut T`.
    pub unsafe fn into_raw(self) -> *mut T {
        let result = self.0.as_ptr();
        core::mem::forget(self);
        result
    }

    // Safety: must have been a validly produced `*mut T`, either by a `VarLenInitializer` call
    // or similar.
    pub unsafe fn from_raw(raw: *mut T) -> Self {
        VBox(NonNull::new_unchecked(raw))
    }
}

impl<T: VarLen> Drop for VBox<T> {
    fn drop(&mut self) {
        unsafe {
            // Careful sequencing of drop:
            //  1. Read the header, before we drop it.
            //  2. Drop the header. Needs to happen before dropping the tail, because there might
            //     be a custom Drop on the header that reads the tail.
            //  3. Drop the tail. Uses the values we read from the header in step 1.
            //  4. Deallocate.
            let size = self.as_mut().size();
            let layout = Layout::from_size_align(size, T::ALIGN).unwrap_or_else(|_| allocation_overflow());
            let drop_tail_fn = self.as_mut().prepare_drop_tail();
            let p = self.0.as_ptr();
            core::ptr::drop_in_place(p);
            drop_tail_fn.drop_tail(self.as_mut());
            std::alloc::dealloc(p as *mut u8, layout);
        }
    }
}

impl<T: VarLen> core::ops::Deref for VBox<T> {
    type Target = T;
    #[inline]
    fn deref(&self) -> &T {
        unsafe { self.0.as_ref() }
    }
}

unsafe impl<T: VarLen> VarLenInitializer<T> for VBox<T> {
    unsafe fn initialize(self, dst: NonNull<T>) {
        let size = self.size();
        let layout = Layout::from_size_align(size, T::ALIGN).unwrap_or_else(|_| allocation_overflow());
        // Safety:
        //  * Owned has unique access to its pointer
        //  * dst is unique
        //  * dst size is guaranteed by the SizedInitializer call
        core::ptr::copy_nonoverlapping(self.0.as_ptr(), dst.as_ptr(), self.size());
        std::alloc::dealloc(self.0.as_ptr() as *mut u8, layout);
        core::mem::forget(self);
    }

    #[inline]
    fn required_size(&self) -> Option<usize> {
        Some(self.size())
    }
}