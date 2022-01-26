#[macro_export]
macro_rules! varlen_newtype {
    (
        #[repr(transparent)]
        $tyvis:vis struct $outer:ident $(< $( ( $($generics:tt)* ) ),* >)? ($fieldvis:vis $inner:ty);

        $(
            with signature: impl< $( ( $($generic_params:tt)* ) ),* > $ignored:ident < $( ($($generics_apply:tt)*) ),* >;
        )?

        with init: $initvis:vis struct $init:ident < _ > ($initfieldvis:vis _);
        with inner_ref: $refvis:vis fn $ref:ident(&self) -> &_;
        with inner_mut: $mutvis:vis fn $mut:ident(self: _) -> _;

    ) => {
        #[repr(transparent)]
        $tyvis struct $outer $(< $($($generics)*),* >)* ($fieldvis $inner);

        $initvis struct $init < InnerInit >($initfieldvis InnerInit);

        unsafe impl $(< $($($generic_params)*),* >)* $crate::VarLen for $outer $(< $($($generics_apply)*),* >)* {
            type Layout = <$inner as $crate::VarLen>::Layout;
            #[inline(always)]
            fn calculate_layout(&self) -> Self::Layout {
                self.0.calculate_layout()
            }

            const ALIGN: usize = <$inner as $crate::VarLen>::ALIGN;
            const NEEDS_DROP_TAIL: bool = <$inner as $crate::VarLen>::NEEDS_DROP_TAIL;

            #[inline(always)]
            unsafe fn drop_tail(self: ::core::pin::Pin<&mut Self>, layout: Self::Layout) {
                <$inner as $crate::VarLen>::drop_tail(self.map_unchecked_mut(|outer| &mut outer.0), layout);
            }
        }

        unsafe impl< $( $($($generic_params)*,)* )* InnerInit: $crate::Initializer<$inner>> $crate::Initializer<$outer $(< $($($generics_apply)*),* >)* > for $init<InnerInit> {
            #[inline(always)]
            fn calculate_layout_cautious(&self) -> ::core::option::Option<<$inner as $crate::VarLen>::Layout> {
                <InnerInit as $crate::Initializer<$inner>>::calculate_layout_cautious(&self.0)
            }

            #[inline(always)]
            unsafe fn initialize(self, dst: ::core::ptr::NonNull<$outer $(< $($($generics_apply)*),* >)* >, layout: <$inner as $crate::VarLen>::Layout) {
                self.0.initialize(dst.cast::<$inner>(), layout);
            }
        }

        impl $(< $($($generic_params)*),* >)* $outer $(< $($($generics_apply)*),* >)* {
            $refvis fn $ref(&self) -> & $inner {
                &self.0
            }

            $mutvis fn $mut(self: ::core::pin::Pin<&mut Self>) -> ::core::pin::Pin<&mut $inner> {
                unsafe {
                    // Safety:
                    // * inner is Unpin if and only if outer is Unpin.
                    // * we don't move out of outer in this lambda.
                    ::core::pin::Pin::map_unchecked_mut(self, |s| &mut s.0)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::array::Array;

    varlen_newtype! {
        #[repr(transparent)]
        pub struct Str(Array<u8>);

        with init: pub struct StrInit<_>(pub _);
        with inner_ref: pub fn inner(&self) -> &_;
        with inner_mut: pub fn inner_mut(self: _) -> _;
    }
}