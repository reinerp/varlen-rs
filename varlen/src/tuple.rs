#![warn(missing_docs)]
#![warn(rustdoc::missing_doc_code_examples)]
#![doc = crate::doc_macro::make_svgbobdoc!(
//! Tuples of variable length types, potentially mixed with fixed-length types.
//! 
//! Tuples of lengths 2-5 are supported. You may nest tuples arbitrarily.
//! 
//! # Examples
//! 
//! A fixed-length type (u32), with two variable-length types:
//! 
//! ```
//! use varlen::prelude::*;
//! type Person = Tup3<FixedLen<u8> /* age */, Str /* name */, Str /* postal address */>;
//! let person: VBox<Person> = VBox::new(tup3::Init(
//!     FixedLen(16),
//!     Str::copy_from_str("Harry Potter"),
//!     Str::copy_from_str("Cupboard under the stairs"),
//! ));
//! assert_eq!(16, person.refs().0.0);
//! assert_eq!("Harry Potter", &person.refs().1[..]);
//! assert_eq!("Cupboard under the stairs", &person.refs().2[..]);
//! ```
//! 
//! # Memory layout
//! 
//! Fields of a tuple are laid out sequentially in memory, in the order they show up in
//! the tuple. Padding is inserted as necessary to satisfy alignment requirements. 
//! For example the layout of the above object on a 64-bit machine is:
//! 
//! ```svgbob
//! +------+---------------------+------------+------------------+---------------------+-------------+-------------------------------+
//! | 16u8 | "<7 bytes padding>" | 12usize    | "'Harry Potter'" | "<4 bytes padding>" | 25usize     | "'Cupboard under the stairs'" |
//! +------+---------------------+------------+------------------+---------------------+-------------+-------------------------------+
//! ```
//! 
//! The implementation of the tuple will *not* reorder fields. You may wish to do so
//! manually, depending on the needs of your application. Here are some common things
//! to keep in mind:
//! 
//! 1. Put fixed-length fields first.
//!
//!    To access the N'th field of a tuple, we generate code to skip over the first (N-1)
//!    fields. Skipping over a fixed-sized field is effectively free (it amounts to a 
//!    constant offset on a pointer, which most instruction sets support very cheaply),
//!    which is why fixed-sized fields should go first. We have done that in the above
//!    example, by putting the [`crate::FixedLen<u8>`] field first.
//! 
//! 2. Put commonly used fields early.
//! 
//!    Earlier fields are more efficient to access than later fields, because we must skip
//!    over early fields in order to access the later fields. So you should put your more
//!    commonly accessed fields first. In the example above, we put the name field before
//!    the address field, which is a good choice if name is more commonly accessed than
//!    address.
//! 
//! 3. Consider how to minimize padding.
//! 
//!    Alignment requirements sometimes cause padding. In the example above, the `usize` 
//!    length fields needed padding inserted before them. This can cause some inefficiency
//!    in memory footprint (the padding bytes take space) and also in access time: we need
//!    to generate code to round up pointers to the appropriate alignment, which takes 
//!    some instructions to calculate. If you can arrange your type to reduce such padding,
//!    you can recover such inefficiencies. One way to do so for `Str` is to use a length
//!    type that has no alignment requirements, such as `Str<u8>`. Another option
//!    is to sort fields from most-aligned to least-aligned.
)]
use crate::marker::FieldMarker;
use crate::{Initializer, VarLen};
use core::concat;
use core::pin::Pin;

macro_rules! define_tuple {
    (
        $(#[$attr:meta])*
        $name:ident< $($arg:ident),* >,
        $mod:ident,
        <$($fieldname:ident),*>,
        <$($offset:ident),*>,
        <$($layout:ident),*>,
        <$($init:ident),*>
    ) => {
        #[doc = concat!("Helper types for [`", stringify!($name), "`].")]
        #[allow(rustdoc::missing_doc_code_examples)]
        pub mod $mod {
            use super::*;

            /// Initializer for a tuple.
            $(#[$attr])*
            pub struct Init<$($arg),*>($(pub $arg),*);

            /// Immutable access to fields of a tuple.
            $(#[$attr])*
            pub struct Refs<'a, $($arg),*>($(pub &'a $arg),*);

            /// Mutable access to fields of a tuple.
            $(#[$attr])*
            pub struct Muts<'a, $($arg),*>($(pub Pin<&'a mut $arg>),*);

            /// Layout of a tuple.
            $(#[$attr])*
            pub struct Layout<$($arg: VarLen),*>{
                pub(super) size: usize,
                $(
                    pub(super) $offset: usize,
                    pub(super) $layout: $arg::Layout
                ),*
            }

            impl<$($arg: VarLen),*> PartialEq for Layout<$($arg),*> {
                #[inline]
                fn eq(&self, other: &Self) -> bool {
                    self.size == other.size
                    $(
                        && self.$offset == other.$offset
                        && self.$layout == other.$layout
                    )*
                }
            }

            impl<$($arg: VarLen),*> Eq for Layout<$($arg),*> {}

            impl<$($arg: VarLen),*> crate::Layout for Layout<$($arg),*> {
                fn size(&self) -> usize {
                    self.size
                }
            }
        }

        /// Tuple of variable-length types.
        $(#[$attr])*
        pub struct $name<$($arg),*>($( pub FieldMarker<$arg> ),* );

        #[allow(rustdoc::missing_doc_code_examples)]
        impl<$($arg: VarLen),*> $name<$($arg),*> {
            /// Immutable access to fields of a tuple.
            $(#[$attr])*
            pub fn refs(&self) -> $mod::Refs<$($arg),*> {
                let layout = self.calculate_layout();
                $mod::Refs($(
                    unsafe { crate::macro_support::ref_field(self, layout.$offset) }
                ),*)
            }

            /// Mutable access to fields of a tuple.
            $(#[$attr])*
            pub fn muts(self: Pin<&mut Self>) -> $mod::Muts<$($arg),*> {
                let layout = self.calculate_layout();
                unsafe {
                    let mut_ptr = self.get_unchecked_mut() as *mut _;
                    $mod::Muts(
                        $(
                            crate::macro_support::mut_field(mut_ptr, layout.$offset)
                        ),*
                    )
                }
            }
        }

        unsafe impl<$($arg: VarLen),*> VarLen for $name<$($arg),*> {
            type Layout = $mod::Layout<$($arg),*>;
            const ALIGN: usize = crate::macro_support::array_max(&[
                $($arg::ALIGN),*
            ]);
            const NEEDS_DROP_TAIL: bool = $(
                $arg::NEEDS_DROP_TAIL || core::mem::needs_drop::<$arg>() ||
            )* false;

            fn calculate_layout(&self) -> $mod::Layout<$($arg),*> {
                let offset = core::mem::size_of::<Self>();
                $(
                    let ($offset, $layout, offset) = crate::macro_support::cat_field_fast::<$arg, _>(self, offset);
                )*
                let size = offset;
                $mod::Layout{size,
                    $( $offset, $layout ),*
                }
            }

            unsafe fn drop_tail(self: Pin<&mut Self>, layout: $mod::Layout<$($arg),*>) {
                let p = self.get_unchecked_mut() as *mut _ as *mut u8;
                $(
                    crate::macro_support::drop_field::<$arg>(p, layout.$offset, layout.$layout);
                )*
            }
        }

        unsafe impl<$($arg: VarLen, $init: Initializer<$arg>),*> Initializer<$name<$($arg),*>> for $mod::Init<$($init),*> {
            fn calculate_layout_cautious(
                &self,
            ) -> Option<$mod::Layout<$($arg),*>> {
                let $mod::Init($($fieldname),*) = self;
                let offset = core::mem::size_of::<$name<$($arg),*>>();
                $(
                    let ($offset, $layout, offset) =
                    crate::macro_support::cat_field_cautious::<$arg, _>($fieldname, offset)?;

                )*
                let size = offset;
                Some($mod::Layout {size,
                    $( $offset, $layout ),*
                })
            }

            unsafe fn initialize(
                self,
                dst: std::ptr::NonNull<$name<$($arg),*>>,
                layout: $mod::Layout<$($arg),*>,
            ) {
                let $mod::Init($($fieldname),*) = self;
                let header = $name(
                    $(
                        crate::macro_support::init_field(
                            $fieldname,
                            dst.cast::<u8>(),
                            layout.$offset,
                            layout.$layout,
                        )
                    ),*
                );
                core::ptr::write(dst.as_ptr(), header);
            }
        }
    }
}

define_tuple!(
    /// # Examples
    /// 
    /// ```
    /// use varlen::prelude::*;
    /// use varlen::Layout;
    /// let mut t: VBox<Tup2<Str, Str>> = VBox::new(tup2::Init(
    ///     Str::copy_from_str("hello"),
    ///     Str::copy_from_str("world"),
    /// ));
    /// 
    /// assert_eq!(&t.refs().0[..], "hello");
    /// assert_eq!(&t.refs().1[..], "world");
    /// t.as_mut().muts().0.mut_slice().make_ascii_uppercase();
    /// assert_eq!(&t.refs().0[..], "HELLO");
    /// assert_eq!(&t.refs().1[..], "world");
    /// 
    /// assert_eq!(t.calculate_layout().size(), 
    ///     core::mem::size_of::<usize>() * 2 + 8 /* hello */ + 5 /* world */);
    /// ```
    Tup2<A, B>, tup2, <t0, t1>, <a_offset, b_offset>, <a_layout, b_layout>, <AInit, BInit>);
define_tuple!(
    /// # Examples
    /// 
    /// ```
    /// use varlen::prelude::*;
    /// use varlen::Layout;
    /// let mut t: VBox<Tup3<Str, Str, Str>> = VBox::new(tup3::Init(
    ///     Str::copy_from_str("hello"),
    ///     Str::copy_from_str("brave"),
    ///     Str::copy_from_str("world"),
    /// ));
    /// 
    /// assert_eq!(&t.refs().0[..], "hello");
    /// assert_eq!(&t.refs().1[..], "brave");
    /// assert_eq!(&t.refs().2[..], "world");
    /// t.as_mut().muts().0.mut_slice().make_ascii_uppercase();
    /// assert_eq!(&t.refs().0[..], "HELLO");
    /// assert_eq!(&t.refs().1[..], "brave");
    /// assert_eq!(&t.refs().2[..], "world");
    /// 
    /// assert_eq!(t.calculate_layout().size(), 
    ///     core::mem::size_of::<usize>() * 3 + 8 /* hello */ + 8 /* brave */ + 5 /* world */);
    /// ```
    Tup3<A, B, C>, tup3, <t0, t1, t2>, <a_offset, b_offset, c_offset>, <a_layout, b_layout, c_layout>, <AInit, BInit, CInit>);
define_tuple!(
    /// # Examples
    /// 
    /// ```
    /// use varlen::prelude::*;
    /// use varlen::Layout;
    /// let mut t: VBox<Tup4<Str, Str, Str, Str>> = VBox::new(tup4::Init(
    ///     Str::copy_from_str("hello"),
    ///     Str::copy_from_str("brave"),
    ///     Str::copy_from_str("new"),
    ///     Str::copy_from_str("world"),
    /// ));
    /// 
    /// assert_eq!(&t.refs().0[..], "hello");
    /// assert_eq!(&t.refs().1[..], "brave");
    /// assert_eq!(&t.refs().2[..], "new");
    /// assert_eq!(&t.refs().3[..], "world");
    /// t.as_mut().muts().0.mut_slice().make_ascii_uppercase();
    /// assert_eq!(&t.refs().0[..], "HELLO");
    /// assert_eq!(&t.refs().1[..], "brave");
    /// assert_eq!(&t.refs().2[..], "new");
    /// assert_eq!(&t.refs().3[..], "world");
    /// 
    /// assert_eq!(t.calculate_layout().size(), 
    ///     core::mem::size_of::<usize>() * 4 + 8 /* hello */ + 8 /* brave */ + 8 /* new */ + 5 /* world */);
    /// ```
    Tup4<A, B, C, D>, tup4, <t0, t1, t2, t3>, <a_offset, b_offset, c_offset, d_offset>, <a_layout, b_layout, c_layout, d_layout>, <AInit, BInit, CInit, DInit>);
define_tuple!(
    /// # Examples
    /// 
    /// ```
    /// use varlen::prelude::*;
    /// use varlen::Layout;
    /// let mut t: VBox<Tup5<Str, Str, Str, Str, Str>> = VBox::new(tup5::Init(
    ///     Str::copy_from_str("hello"),
    ///     Str::copy_from_str("brave"),
    ///     Str::copy_from_str("new"),
    ///     Str::copy_from_str("world"),
    ///     Str::copy_from_str("!!!"),
    /// ));
    /// 
    /// assert_eq!(&t.refs().0[..], "hello");
    /// assert_eq!(&t.refs().1[..], "brave");
    /// assert_eq!(&t.refs().2[..], "new");
    /// assert_eq!(&t.refs().3[..], "world");
    /// assert_eq!(&t.refs().4[..], "!!!");
    /// t.as_mut().muts().0.mut_slice().make_ascii_uppercase();
    /// assert_eq!(&t.refs().0[..], "HELLO");
    /// assert_eq!(&t.refs().1[..], "brave");
    /// assert_eq!(&t.refs().2[..], "new");
    /// assert_eq!(&t.refs().3[..], "world");
    /// assert_eq!(&t.refs().4[..], "!!!");
    /// 
    /// assert_eq!(t.calculate_layout().size(), 
    ///     core::mem::size_of::<usize>() * 5 + 8 /* hello */ + 8 /* brave */ + 8 /* new */ + 8 /* world */ + 3 /* !!! */);
    /// ```
    Tup5<A, B, C, D, E>, tup5, <t0, t1, t2, t3, t4>, <a_offset, b_offset, c_offset, d_offset, e_offset>, <a_layout, b_layout, c_layout, d_layout, e_layout>, <AInit, BInit, CInit, DInit, EInit>);
