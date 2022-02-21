//! Single module with almost all `varlen` exports
//!
//! # Examples
//!
//! Import all into current scope:
//!
//! ```
//! use varlen::prelude::*;
//! ```
//!
//! Import all, named:
//!
//! ```
//! use varlen::prelude as v;
//! ```

pub use crate::array::{Array, ArrayLen, SizedInit};
pub use crate::array_init::{
    new_array, ArrayInitializer, CloneFrom, CopyFrom, FillSequentially, FillWithDefault,
    FromIterPrefix, MoveFrom,
};
#[cfg(feature = "macro")]
pub use crate::define_varlen;
pub use crate::newtype::{define_varlen_newtype, impl_initializer_as_newtype};
pub use crate::owned::Owned;
pub use crate::seq::{seq, IndexableSeq, Indexing, Seq};
pub use crate::str::Str;
pub use crate::tuple::{tup2, tup3, tup4, tup5, Tup2, Tup3, Tup4, Tup5};
pub use crate::vbox::VBox;
pub use crate::{FixedLen, Initializer, VClone, VCopy, VarLen};
