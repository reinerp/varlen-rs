#![allow(dead_code)]
use crate::{Initializer, Layout, VarLen};
use crate::array_init::ArrayInitializer;
use core::mem::MaybeUninit;
use core::pin::Pin;
use core::ptr::NonNull;

pub struct SingleByteArray {
    len: usize,
    // Tail: [u8; len]
}

impl SingleByteArray {
    pub fn tail(&self) -> &[u8] {
        let len = self.len;
        unsafe {
            let ptr = (self as *const SingleByteArray).add(1) as *const u8;
            core::slice::from_raw_parts(ptr, len)
        }
    }

    pub fn tail_mut(self: Pin<&mut Self>) -> &mut [u8] {
        let len = self.len;
        unsafe {
            let ptr = (self.get_unchecked_mut() as *mut SingleByteArray).add(1) as *mut u8;
            core::slice::from_raw_parts_mut(ptr, len)
        }
    }
}

pub struct SBALayout {
    array_len: usize,
    size: usize,
}

impl Layout for SBALayout {
    fn size(&self) -> usize {
        self.size
    }
}

unsafe impl VarLen for SingleByteArray {
    type Layout = SBALayout;

    fn calculate_layout(&self) -> SBALayout {
        let tail_offset = core::mem::size_of::<Self>();
        let array_len = self.len;
        let size = tail_offset + array_len * core::mem::size_of::<u8>();
        SBALayout { array_len, size }
    }

    const ALIGN: usize = core::mem::align_of::<Self>();

    const NEEDS_DROP_TAIL: bool = false;

    unsafe fn drop_tail(self: core::pin::Pin<&mut Self>, _layout: SBALayout) {}
}

pub struct SingleByteArrayInit<TailInit> {
    pub len: usize,
    pub tail: TailInit,
}

unsafe impl<TailInit: ArrayInitializer<u8>> Initializer<SingleByteArray>
    for SingleByteArrayInit<TailInit>
{
    fn calculate_layout_cautious(&self) -> Option<SBALayout> {
        let tail_offset = core::mem::size_of::<Self>();
        let array_len = self.len;
        let size = tail_offset.checked_add(self.len)?;
        Some(SBALayout { array_len, size })
    }

    unsafe fn initialize(self, dst: NonNull<SingleByteArray>, layout: SBALayout) {
        core::ptr::write(dst.as_ptr(), SingleByteArray { len: self.len });
        let tail = core::slice::from_raw_parts_mut(
            dst.as_ptr().add(1) as *mut MaybeUninit<u8>,
            layout.array_len,
        );
        self.tail.initialize(tail)
    }
}
