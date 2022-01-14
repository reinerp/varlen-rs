use crate::{VarLen, SizedInitializer};
use core::ptr::NonNull;

pub struct Seq<T: VarLen> {
    ptr: NonNull<T>,
    len_logical: usize,
    occupied: usize,
    capacity: usize,
}

impl<T: VarLen> Seq<T> {
    pub const fn new() -> Self {
        Seq{
            ptr: NonNull::dangling(),
            len_logical: 0,
            occupied: 0,
            capacity: 0,
        }
    }

    pub const fn len(&self) -> usize {
        self.len_logical
    }

    pub fn push(&mut self, init: impl SizedInitializer<T>) {
        let layout = init.layout().unwrap_or_else(|| panic!("Overflowing layout"));
        
    }
}

struct SeqSlicePtr<T: VarLen>{
    ptr: NonNull<T>,
    len_logical: usize,
}

pub struct SeqSlice<T: VarLen>(SeqSlicePtr<T>);

pub struct SeqSliceMut<T: VarLen>(SeqSlicePtr<T>);