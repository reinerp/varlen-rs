use super::{Initializer, Layout, VarLen};

use core::alloc;
use core::pin::Pin;
use core::ptr::NonNull;

/// [`Box<T>`], but for [`VarLen`] types `T`.
pub struct VBox<T: VarLen>(NonNull<T>);

#[inline(never)]
#[cold]
fn allocation_overflow() -> ! {
    panic!("Allocation size overflow")
}

impl<T: VarLen> VBox<T> {
    pub fn new(init: impl Initializer<T>) -> Self {
        let layout = init
            .calculate_layout_cautious()
            .unwrap_or_else(|| allocation_overflow());
        let alloc_layout = alloc::Layout::from_size_align(layout.size(), T::ALIGN)
            .unwrap_or_else(|_| allocation_overflow());
        unsafe {
            let p = std::alloc::alloc(alloc_layout) as *mut T;
            let layout_size = layout.size();
            init.initialize(NonNull::new_unchecked(p), layout);
            let mut p = NonNull::new_unchecked(p);
            // TODO(reinerp): Compare directly on Layout type? Or too much generated code?
            debug_assert_eq!(p.as_mut().calculate_layout().size(), layout_size);
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
            //  1. Read the layout, before we drop it.
            //  2. Drop the header. Needs to happen before dropping the tail, because there might
            //     be a custom Drop on the header that reads the tail.
            //  3. Drop the tail. Uses the values we read from the header in step 1.
            //  4. Deallocate.
            let layout = T::calculate_layout(&*self);
            let alloc_layout = alloc::Layout::from_size_align_unchecked(layout.size(), T::ALIGN);
            let p = self.0.as_ptr();
            core::ptr::drop_in_place(p);
            T::drop_tail(self.as_mut(), layout);
            std::alloc::dealloc(p as *mut u8, alloc_layout);
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

unsafe impl<T: VarLen> Initializer<T> for VBox<T> {
    unsafe fn initialize(self, dst: NonNull<T>, layout: T::Layout) {
        let size = layout.size();
        // Safety: we already called from_size_align in the VBox constructor.
        let layout = alloc::Layout::from_size_align_unchecked(size, T::ALIGN);
        // Safety:
        //  * Owned has unique access to its pointer
        //  * dst is unique
        //  * dst size is guaranteed by the SizedInitializer call
        core::ptr::copy_nonoverlapping(
            self.0.as_ptr().cast::<u8>(),
            dst.as_ptr().cast::<u8>(),
            size,
        );
        std::alloc::dealloc(self.0.as_ptr() as *mut u8, layout);
        core::mem::forget(self);
    }

    #[inline]
    fn calculate_layout_cautious(&self) -> Option<T::Layout> {
        Some(T::calculate_layout(&*self))
    }
}
