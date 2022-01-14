use super::{DropTailFn, SizedInitializer, VarLen};

use core::alloc::Layout;
use core::ptr::NonNull;
use core::pin::Pin;

/// Box<T>, but for VarLen types T.
pub struct Box<T: VarLen>(NonNull<T>);

#[inline(never)]
fn allocation_overflow() -> ! {
    panic!("Allocation size overflow")
}

impl<T: VarLen> Box<T> {
    pub fn new(init: impl SizedInitializer<T>) -> Self {
        let size = init.size().unwrap_or_else(|| allocation_overflow());
        let layout = Layout::from_size_align(size, T::ALIGN).unwrap_or_else(|_| allocation_overflow());
        unsafe {
            let p = std::alloc::alloc(layout) as *mut T;
            init.initialize(NonNull::new_unchecked(p));
            let mut p=  NonNull::new_unchecked(p);
            debug_assert_eq!(p.as_mut().size(), size);
            Box(p)
        }
    }

    pub fn as_mut(&mut self) -> Pin<&mut T> {
        unsafe { Pin::new_unchecked(self.0.as_mut()) }
    }

    // Safety: must not be used to produce a `&mut T`.
    pub unsafe fn into_raw(self) -> *mut T {
        self.0.as_ptr()
    }

    // Safety: must have been a validly produced `*mut T`, either by a `SizedInitializer` call
    // or similar.
    pub unsafe fn from_raw(raw: *mut T) -> Self {
        Box(NonNull::new_unchecked(raw))
    }
}

impl<T: VarLen> Drop for Box<T> {
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

impl<T: VarLen> core::ops::Deref for Box<T> {
    type Target = T;
    fn deref(&self) -> &T {
        unsafe { self.0.as_ref() }
    }
}