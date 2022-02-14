// TODO: Rephrase in terms of Pair<A: VarLen, B: VarLen> and `FixLen<T>: VarLen`.
//
// So:
//   GenericVarLen1<Head, Tail> = Pair<FixLen<Head>, Tail>
//   GenericVarLen2<Head, Tail1, Tail2> = Pair<FixLen<Head>, Pair<Tail1, Tail2>>;
//
// Or possibly: Tup2<T, U>, Tup3<T, U, V>, Tup4<T, U, V, W>

use crate::{Initializer, VarLen};
use core::pin::Pin;

/// Generic implementation of a varlen type plus a fixed-length header.
pub struct GenericVarLen1<Head, Tail> {
    pub head: Head,
    pub tail: crate::marker::FieldMarker<Tail>,
}

pub mod generic_var_len_1 {
    use core::pin::Pin;

    /// Initializer for `GenericVarLen1`
    pub struct Init<Head, TailInit> {
        pub head: Head,
        pub tail: TailInit,
    }

    /// Layout of `GenericVarLen1`
    pub struct Layout<TailLayout> {
        pub(super) tail_offset: usize,
        pub(super) tail_layout: TailLayout,
        pub(super) size: usize,
    }

    /// Immutable references to fields in `GenericVarLen1`
    pub struct Refs<'a, Head, Tail> {
        pub head: &'a Head,
        pub tail: &'a Tail,
    }

    /// Mutable references to fields in `GenericVarLen1`
    pub struct Muts<'a, Head, Tail> {
        pub head: &'a mut Head,
        pub tail: Pin<&'a mut Tail>,
    }

    impl<TailLayout: crate::Layout> crate::Layout for Layout<TailLayout> {
        #[inline(always)]
        fn size(&self) -> usize {
            self.size
        }
    }
}

unsafe impl<Head, Tail: VarLen> VarLen for GenericVarLen1<Head, Tail> {
    type Layout = generic_var_len_1::Layout<Tail::Layout>;
    #[inline(always)]
    fn calculate_layout(&self) -> Self::Layout {
        let offset = core::mem::size_of::<Self>();
        let (tail_offset, tail_layout, size) =
            crate::macro_support::cat_field_fast::<Tail, _>(self, offset);
        generic_var_len_1::Layout {
            tail_offset,
            tail_layout: tail_layout,
            size,
        }
    }

    const ALIGN: usize =
        crate::macro_support::array_max(&[core::mem::align_of::<Head>(), Tail::ALIGN]);

    const NEEDS_DROP_TAIL: bool = core::mem::needs_drop::<Tail>() || Tail::NEEDS_DROP_TAIL;

    #[inline(always)]
    unsafe fn drop_tail(self: Pin<&mut Self>, layout: Self::Layout) {
        crate::macro_support::drop_field::<Tail>(
            self.get_unchecked_mut() as *mut _ as *mut u8,
            layout.tail_offset,
            layout.tail_layout,
        );
    }
}

impl<Head, Tail: VarLen> GenericVarLen1<Head, Tail> {
    /// Gets immutable access to the fields.
    #[inline(always)]
    pub fn refs(&self) -> generic_var_len_1::Refs<Head, Tail> {
        let layout = self.calculate_layout();
        generic_var_len_1::Refs {
            head: &self.head,
            tail: unsafe { crate::macro_support::ref_field(self, layout.tail_offset) },
        }
    }

    /// Immutable access to the tail.
    #[inline(always)]
    pub fn tail(&self) -> &Tail {
        self.refs().tail
    }

    /// Gets mutable access to the fields.
    #[inline(always)]
    pub fn muts(self: Pin<&mut Self>) -> generic_var_len_1::Muts<Head, Tail> {
        let layout = self.calculate_layout();
        unsafe {
            let mut_ref = self.get_unchecked_mut();
            let mut_ptr = mut_ref as *mut _;
            generic_var_len_1::Muts {
                head: &mut mut_ref.head,
                tail: crate::macro_support::mut_field(mut_ptr, layout.tail_offset),
            }
        }
    }

    /// Mutable access to the tail.
    #[inline(always)]
    pub fn tail_mut(self: Pin<&mut Self>) -> Pin<&mut Tail> {
        self.muts().tail
    }
}

unsafe impl<Head, Tail: VarLen, TailInit: Initializer<Tail>> Initializer<GenericVarLen1<Head, Tail>>
    for generic_var_len_1::Init<Head, TailInit>
{
    fn calculate_layout_cautious(&self) -> Option<generic_var_len_1::Layout<Tail::Layout>> {
        let offset = core::mem::size_of::<Head>();
        let (tail_offset, tail_layout, size) =
            crate::macro_support::cat_field_cautious::<Tail, _>(&self.tail, offset)?;
        Some(generic_var_len_1::Layout {
            tail_offset,
            tail_layout: tail_layout,
            size,
        })
    }

    unsafe fn initialize(
        self,
        dst: std::ptr::NonNull<GenericVarLen1<Head, Tail>>,
        layout: generic_var_len_1::Layout<Tail::Layout>,
    ) {
        let header = GenericVarLen1 {
            head: self.head,
            tail: crate::macro_support::init_field(
                self.tail,
                dst.cast::<u8>(),
                layout.tail_offset,
                layout.tail_layout,
            ),
        };
        core::ptr::write(dst.as_ptr(), header);
    }
}

/// Generic implementation of a varlen type plus a fixed-length header.
pub struct GenericVarLen2<Head, Tail1, Tail2> {
    pub head: Head,
    pub tail1: crate::marker::FieldMarker<Tail1>,
    pub tail2: crate::marker::FieldMarker<Tail2>,
}

pub mod generic_var_len_2 {
    use core::pin::Pin;

    /// Initializer for `GenericVarLen2`
    pub struct Init<Head, Tail1Init, Tail2Init> {
        pub head: Head,
        pub tail1: Tail1Init,
        pub tail2: Tail2Init,
    }

    /// Layout of `GenericVarLen2`
    pub struct Layout<Tail1Layout, Tail2Layout> {
        pub(super) tail1_offset: usize,
        pub(super) tail1_layout: Tail1Layout,
        pub(super) tail2_offset: usize,
        pub(super) tail2_layout: Tail2Layout,
        pub(super) size: usize,
    }

    /// Immutable references to fields in `GenericVarLen1`
    pub struct Refs<'a, Head, Tail1, Tail2> {
        pub head: &'a Head,
        pub tail1: &'a Tail1,
        pub tail2: &'a Tail2,
    }

    /// Mutable references to fields in `GenericVarLen1`
    pub struct Muts<'a, Head, Tail1, Tail2> {
        pub head: &'a mut Head,
        pub tail1: Pin<&'a mut Tail1>,
        pub tail2: Pin<&'a mut Tail2>,
    }

    impl<Tail1Layout: crate::Layout, Tail2Layout: crate::Layout> crate::Layout
        for Layout<Tail1Layout, Tail2Layout>
    {
        #[inline(always)]
        fn size(&self) -> usize {
            self.size
        }
    }
}

unsafe impl<Head, Tail1: VarLen, Tail2: VarLen> VarLen for GenericVarLen2<Head, Tail1, Tail2> {
    type Layout = generic_var_len_2::Layout<Tail1::Layout, Tail2::Layout>;
    #[inline(always)]
    fn calculate_layout(&self) -> Self::Layout {
        let offset = core::mem::size_of::<Self>();
        let (tail1_offset, tail1_layout, offset) =
            crate::macro_support::cat_field_fast::<Tail1, _>(self, offset);
        let (tail2_offset, tail2_layout, offset) =
            crate::macro_support::cat_field_fast::<Tail2, _>(self, offset);
        let size = offset;
        generic_var_len_2::Layout {
            tail1_offset,
            tail2_offset,
            tail1_layout,
            tail2_layout,
            size,
        }
    }

    const ALIGN: usize = crate::macro_support::array_max(&[
        core::mem::align_of::<Head>(),
        Tail1::ALIGN,
        Tail2::ALIGN,
    ]);

    const NEEDS_DROP_TAIL: bool = core::mem::needs_drop::<Tail1>()
        || Tail1::NEEDS_DROP_TAIL
        || core::mem::needs_drop::<Tail2>()
        || Tail2::NEEDS_DROP_TAIL;

    #[inline(always)]
    unsafe fn drop_tail(self: Pin<&mut Self>, layout: Self::Layout) {
        let p = self.get_unchecked_mut() as *mut _ as *mut u8;
        crate::macro_support::drop_field::<Tail1>(p, layout.tail1_offset, layout.tail1_layout);
        crate::macro_support::drop_field::<Tail2>(p, layout.tail2_offset, layout.tail2_layout);
    }
}

impl<Head, Tail1: VarLen, Tail2: VarLen> GenericVarLen2<Head, Tail1, Tail2> {
    /// Gets immutable access to the fields.
    #[inline(always)]
    pub fn refs(&self) -> generic_var_len_2::Refs<Head, Tail1, Tail2> {
        let layout = self.calculate_layout();
        generic_var_len_2::Refs {
            head: &self.head,
            tail1: unsafe { crate::macro_support::ref_field(self, layout.tail1_offset) },
            tail2: unsafe { crate::macro_support::ref_field(self, layout.tail2_offset) },
        }
    }

    /// Immutable access to the tail1.
    #[inline(always)]
    pub fn tail1(&self) -> &Tail1 {
        self.refs().tail1
    }

    /// Immutable access to the tail1.
    #[inline(always)]
    pub fn tail2(&self) -> &Tail2 {
        self.refs().tail2
    }

    /// Gets mutable access to the fields.
    #[inline(always)]
    pub fn muts(self: Pin<&mut Self>) -> generic_var_len_2::Muts<Head, Tail1, Tail2> {
        let layout = self.calculate_layout();
        unsafe {
            let mut_ref = self.get_unchecked_mut();
            let mut_ptr = mut_ref as *mut _;
            generic_var_len_2::Muts {
                head: &mut mut_ref.head,
                tail1: crate::macro_support::mut_field(mut_ptr, layout.tail1_offset),
                tail2: crate::macro_support::mut_field(mut_ptr, layout.tail2_offset),
            }
        }
    }

    /// Mutable access to the tail1.
    #[inline(always)]
    pub fn tail1_mut(self: Pin<&mut Self>) -> Pin<&mut Tail1> {
        self.muts().tail1
    }

    /// Mutable access to the tail1.
    #[inline(always)]
    pub fn tail2_mut(self: Pin<&mut Self>) -> Pin<&mut Tail2> {
        self.muts().tail2
    }
}

unsafe impl<
        Head,
        Tail1: VarLen,
        Tail1Init: Initializer<Tail1>,
        Tail2: VarLen,
        Tail2Init: Initializer<Tail2>,
    > Initializer<GenericVarLen2<Head, Tail1, Tail2>>
    for generic_var_len_2::Init<Head, Tail1Init, Tail2Init>
{
    fn calculate_layout_cautious(
        &self,
    ) -> Option<generic_var_len_2::Layout<Tail1::Layout, Tail2::Layout>> {
        let offset = core::mem::size_of::<Head>();
        let (tail1_offset, tail1_layout, offset) =
            crate::macro_support::cat_field_cautious::<Tail1, _>(&self.tail1, offset)?;
        let (tail2_offset, tail2_layout, offset) =
            crate::macro_support::cat_field_cautious::<Tail2, _>(&self.tail2, offset)?;
        let size = offset;
        Some(generic_var_len_2::Layout {
            tail1_offset,
            tail1_layout: tail1_layout,
            tail2_offset,
            tail2_layout: tail2_layout,
            size,
        })
    }

    unsafe fn initialize(
        self,
        dst: std::ptr::NonNull<GenericVarLen2<Head, Tail1, Tail2>>,
        layout: generic_var_len_2::Layout<Tail1::Layout, Tail2::Layout>,
    ) {
        let p = dst.cast::<u8>();
        let header = GenericVarLen2 {
            head: self.head,
            tail1: crate::macro_support::init_field(
                self.tail1,
                p,
                layout.tail1_offset,
                layout.tail1_layout,
            ),
            tail2: crate::macro_support::init_field(
                self.tail2,
                p,
                layout.tail2_offset,
                layout.tail2_layout,
            ),
        };
        core::ptr::write(dst.as_ptr(), header);
    }
}
