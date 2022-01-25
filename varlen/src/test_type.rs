// use crate::{VarLen, VarLenInitializer, Layout, ArrayInitializer};
// use core::pin::Pin;
// use core::ptr::NonNull;
// use core::mem::MaybeUninit;

// pub struct SingleByteArray {
//     len: usize,
//     // Tail: [u8; len]
// }

// impl SingleByteArray {
//     pub fn tail(&self) -> &[u8] {
//         let len = self.len;
//         unsafe {
//             let ptr = (self as *const SingleByteArray).add(1) as *const u8;
//             core::slice::from_raw_parts(ptr, len)
//         }
//     }

//     pub fn tail_mut(&mut self) -> &mut [u8] {
//         let len = self.len;
//         unsafe {
//             let ptr = (self as *mut SingleByteArray).add(1) as *mut u8;
//             core::slice::from_raw_parts_mut(ptr, len)
//         }
//     }
// }

// unsafe impl VarLen for SingleByteArray {
//     fn size(&self) -> usize {
//         core::mem::size_of::<Self>() + self.len
//     }

//     const ALIGN: usize = core::mem::align_of::<Self>();

//     const NEEDS_DROP_TAIL: bool = false;

//     type DropTailFn = NothingToDrop;

//     fn prepare_drop_tail(&self) -> Self::DropTailFn {
//         NothingToDrop
//     }
// }

// pub struct NothingToDrop;

// impl<T> DropTailFn<T> for NothingToDrop {
//     unsafe fn drop_tail(self, _obj: Pin<&mut T>) {}
// }

// pub struct SingleByteArrayInit<TailInit> {
//     pub len: usize,
//     pub tail: TailInit,
// }

// unsafe impl<TailInit: ArrayInitializer<u8>> VarLenInitializer<SingleByteArray> for SingleByteArrayInit<TailInit> {
//     fn required_size(&self) -> Option<usize> {
//         core::mem::size_of::<SingleByteArray>().checked_add(self.len)
//     }

//     unsafe fn initialize(self, dst: NonNull<SingleByteArray>) {
//         core::ptr::write(dst.as_ptr(), SingleByteArray{len: self.len});
//         let tail = core::slice::from_raw_parts_mut(dst.as_ptr().add(1) as *mut MaybeUninit<u8>, self.len) 
//             as *mut [MaybeUninit<u8>] as *mut [u8];
//         self.tail.initialize(NonNull::new_unchecked(tail))
//     }
// }