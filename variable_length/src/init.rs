use super::ArrayInitializer;
use core::ptr::NonNull;
use core::mem::MaybeUninit;

/// Substitute for NonNull::as_uninit_slice_mut() until that stabilizes.
unsafe fn as_uninit_slice_mut<'a, T>(p: NonNull<[T]>) -> &'a mut [MaybeUninit<T>] {
    NonNull::new_unchecked(p.as_ptr() as *mut [MaybeUninit<T>]).as_mut()
}

pub struct FillSequentially<Lambda>(pub Lambda);

unsafe impl<T, Lambda: FnMut(usize) -> T> ArrayInitializer<T> for FillSequentially<Lambda> {
    unsafe fn initialize(mut self, dst: NonNull<[T]>) {
        let dst= as_uninit_slice_mut(dst);
        for (i, slot) in dst.iter_mut().enumerate() {
            slot.write(self.0(i));
        }
    }
}

pub struct FillWithDefault();

unsafe impl<T: Default> ArrayInitializer<T> for FillWithDefault {
    unsafe fn initialize(self, dst: NonNull<[T]>) {
        FillSequentially(|_i| Default::default()).initialize(dst)
    }
}

pub struct CopyFrom<'a, T>(pub &'a [T]);

unsafe impl<'a, T: Copy> ArrayInitializer<T> for CopyFrom<'a, T> {
    unsafe fn initialize(self, dst: NonNull<[T]>) {
        // TODO(reinerp): Use MaybeUninit::write_slice once it stabilizes.
        let dst = as_uninit_slice_mut(dst);
        // Safety: MaybeUninit<T> and T have the same layout.
        let src: &[MaybeUninit<T>] = core::mem::transmute(self.0);
        dst.copy_from_slice(src)
    }
}

pub struct CloneFrom<'a, T>(pub &'a [T]);

unsafe impl<'a, T: Clone> ArrayInitializer<T> for CloneFrom<'a, T> {
    unsafe fn initialize(self, dst: NonNull<[T]>) {
        assert_eq!(self.0.len(), as_uninit_slice_mut(dst).len());
        FillSequentially(|i| (&self.0[i] as &T).clone()).initialize(dst)
    }
}

pub struct MoveFrom<T, const N: usize>(pub [T; N]);

unsafe impl<T, const N: usize> ArrayInitializer<T> for MoveFrom<T, N> {
    unsafe fn initialize(self, dst: NonNull<[T]>) {
        assert_eq!(as_uninit_slice_mut(dst).len(), N);
        core::ptr::write(dst.cast::<[T; N]>().as_ptr(), self.0)
    }
}
