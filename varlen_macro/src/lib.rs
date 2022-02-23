//! Macro `#[define_varlen]` for defining variable-length structs.
//! 
//! See [`varlen` crate](https://docs.rs/varlen).

use std::collections::HashSet;

use convert_case::{Case, Casing};
use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{parse_quote, Generics, GenericParam, Lifetime};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::Comma;
use syn::{Attribute, Data, DeriveInput, Field, Fields, Ident, Type, Visibility};

/// Macro for defining variable-length structs.
///
/// # Examples
///
/// A struct with multiple variable-length fields, specified by `#[varlen]`:
///
/// ```
/// use varlen::prelude::*;
/// #[define_varlen]
/// struct Person {
///     age: usize,
///     #[varlen]
///     name: Str,
///     #[varlen]
///     email: Str,
/// }
/// # fn main() {
/// let mut p: VBox<Person> = VBox::new(
///     person::Init{
///         age: 16,
///         name: Str::copy("Harry Potter"),
///         email: Str::copy("harry.potter@example.com"),
///     }
/// );
/// assert_eq!(p.age, 16);
/// assert_eq!(&p.refs().name[..], "Harry Potter");
/// p.as_mut().muts().name.mut_slice().make_ascii_uppercase();
/// assert_eq!(&p.refs().name[..], "HARRY POTTER");
/// assert_eq!(&p.refs().email[..], "harry.potter@example.com");
/// # }
/// ```
///
/// A struct with variable-length arrays. The arrays are annotated with
/// `#[varlen_array]`. You may directly specify the array lengths as
/// any `const`-evaluatable expression that references the fields
/// annotated with `#[controls_layout]`:
///
/// ```
/// use varlen::prelude::*;
/// #[define_varlen]
/// struct MultipleArrays {
///     #[controls_layout]
///     len: usize,
///
///     #[varlen_array]
///     array1: [u16; *len],
///
///     #[varlen_array]
///     array2: [u8; *len],
///
///     #[varlen_array]
///     half_array: [u16; (*len) / 2],
/// }
/// # fn main() {
/// let base_array = vec![1, 3, 9, 27];
/// let a: VBox<MultipleArrays> = VBox::new(multiple_arrays::Init{
///     len: base_array.len(),
///     array1: FillSequentially(|i| base_array[i]),
///     array2: FillSequentially(|i| base_array[base_array.len() - 1 - i] as u8),
///     half_array: FillSequentially(|i| base_array[i * 2]),
/// });
/// assert_eq!(a.refs().array1, &[1, 3, 9, 27]);
/// assert_eq!(a.refs().array2, &[27, 9, 3, 1]);
/// assert_eq!(a.refs().half_array, &[1, 9]);
/// # }
/// ```
///
/// # Attributes
///
/// The following attributes are available on fields:
///
/// * `#[varlen]` - use this for a field type which implements `VarLen`
/// * `#[varlen_array]` - use this for an array-typed field. The length
///    of the array may refer to fields annotated with
///    `#[controls_layout]`, and must be a `const`-evaluatable expression
/// * `#[controls_layout]` - this marks a field as usable to specify the
///   length of a `#[varlen_array]` field. It also removes mutable access
///   to the field. Mutable access risks causing memory unsafety, in which
///   the length of the array is recorded incorrectly in memory.
///
/// # Generated API
///
/// See crate [`varlen_generated`](https://docs.rs/varlen_example) for an example of the generated code.
///
/// For a struct `MyType`, the following is generated:
///
/// * A type `MyType`, with variable-length fields replaced with
///   marker types `varlen::marker::FieldMarker` and `varlen::marker::ArrayMarker`
/// * A module `my_type` (the snake-case version of the struct name), containing:
///   * a type `VarLenLayout` which specifies the layout of `MyType`
///   * a type `Refs<'a>` consisting of immutable references, with lifetime `'a`
///   * a type `Muts<'a>` consisting of mutable references, with lifetime `'a`
///   * a type `Init<...>` which implements `varlen::Initializer<MyType>`
///   * a type `LayoutControllers`, which consists of the fields annotated `#[controls_layout]`
/// * Various member functions on `MyType`, for immutable and mutable access.
/// * Trait implementations for `MyType`:
///   * `VarLen` for `MyType`
///   * `Drop` for `MyType`, which unconditionally panics.
///
/// # Memory layout
///
/// The object is laid out in memory with the fixed-length fields first. These are
/// the fields which are not annotated with `#[varlen]` or `#[varlen_array]`. The
/// fixed-length fields are laid out in whatever order is chosen by the Rust compiler.
///
/// The variable-length fields always follow the fixed-length fields in memory. The
/// variable-length fields are always laid out in the order specified in the struct
/// definition. Padding is inserted between variable-length fields as needed to
/// meet the alignment requirements of each field.
///
/// Padding before variable-length fields costs both memory and time. To minimize
/// padding effects, as much as possible try to use the same alignment for all
/// fields. Where this is not possible, try to order your fields from most-aligned
/// to least-aligned: when a less-aligned field follows a more-aligned field, there
/// are zero padding bytes, and the padding computation can be entirely optimized
/// away.
///
/// Access to a variable-length field requires running some code at runtime that
/// skips over all previous variable-length fields in the struct. To minimize time
/// spent on skipping over these fields, you can:
///
/// * Store the result of `refs()` in a local variable, and reuse that variable
///   multiple times. This calculates the beginning of all variable-length fields
///   once, and then reuses that calculation.
/// * Sort the variable-length fields of the struct so that the
///   most-frequently-used fields come first.
#[proc_macro_attribute]
pub fn define_varlen(
    attrs: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let attrs: proc_macro2::TokenStream = attrs.into();
    let item: proc_macro2::TokenStream = item.into();
    match define_varlen_impl(attrs.clone(), item.clone()) {
        Ok(x) => x,
        Err(Error(msg, span)) => {
            let error = quote_spanned!(span =>
                ::core::compile_error!(#msg);
            );
            quote!(
                #attrs
                #item
                #error
            )
        }
    }
    .into()
}

struct Error(&'static str, proc_macro2::Span);

fn define_varlen_impl(ty_attrs: TokenStream, d: TokenStream) -> Result<TokenStream, Error> {
    let d: DeriveInput = match syn::parse2(d) {
        Ok(d) => d,
        Err(e) => {
            return Err(Error(
                "define_varlen could not parse this as a struct",
                e.span(),
            ))
        }
    };
    let d_attrs = d.attrs;
    let fields = if let Data::Struct(s) = d.data {
        if let Fields::Named(n) = s.fields {
            n.named
        } else {
            return Err(Error("define_varlen requires named fields", d.ident.span()));
        }
    } else {
        return Err(Error("define_varlen requires a struct", d.ident.span()));
    };

    let (tyvis, tyvis_inner) = SimpleVisibility::try_parse(&d.vis)?;
    let tyname = d.ident;

    let mut mod_name = format_ident!("{}", tyname.to_string().to_case(Case::Snake));
    mod_name.set_span(proc_macro2::Span::call_site());

    let FieldGroups {
        lengths:
            LengthFields {
                meta:
                    Meta {
                        ident: lengths_ident,
                        attrs: lengths_attr,
                        vis_inner: lengths_vis_inner,
                        ..
                    },
                tys: lengths_ty,
            },
        len_exprs: LengthExprs {
            len_expr,
            len_ident,
        },
        varlen_fields:
            FieldMarkers {
                meta:
                    Meta {
                        ident: varlen_ident,
                        ..
                    },
                init_ty_params: varlen_init_ty_param,
                init_ty_constraints: varlen_init_ty_constraint,
                layout_idents: varlen_layout_ident,
                layout_ty: varlen_layout_ty,
                layout_ty_refs: varlen_layout_ty_refs,
                cat_field_fast: varlen_cat_field_fast,
                cat_field_cautious: varlen_cat_field_cautious,
                align_of_field: varlen_align_of_field,
            },
        all_fields:
            AllFields {
                meta:
                    Meta {
                        attrs: all_attr,
                        ident: all_ident,
                        vis: all_vis,
                        vis_inner: all_vis_inner,
                    },
                decl_ty: all_decl_ty,
                init_ty: all_init_ty,
                init_field: all_init_field,
                ref_ty: all_ref_ty,
                mut_ty: all_mut_ty,
                ref_field: all_ref_field,
                mut_field: all_mut_field,
                drop_field: all_drop_field,
                needs_vdrop: all_needs_vdrop,
            },
    } = parse_fields(fields, &mod_name)?;

    let (generics_structdef, generics_impl, generics_apply, generics_where) 
        = parse_generics(&d.generics, None);
    let (layout_generics_structdef, layout_generics_impl, layout_generics_apply, layout_generics_where) 
        = parse_generics(&d.generics, Some(varlen_layout_ty_refs));

    // TODO(reinerp): This is potentially quadratic syntax size. Create type synonym instead,
    // to keep it linear.
    let len_fn_arg = quote! {
        (
            #(
                #lengths_ident,
            )*
        ): (
            #(
                & #lengths_ty,
            )*
        )
    };

    Ok(quote! {
        #tyvis mod #mod_name {
            use super::*;

            /// Array offsets and lengths for all trailing arrays and nested types.
            #tyvis_inner struct VarLenLayout<#(#layout_generics_structdef),*> #layout_generics_where {
                pub(super) size: usize,
                #(
                    pub(super) #varlen_ident: usize,
                    pub(super) #varlen_layout_ident: #varlen_layout_ty,
                )*
            }

            impl<#(#layout_generics_impl),*> ::varlen::Layout for VarLenLayout<#(#layout_generics_apply),*> #layout_generics_where {
                #[inline(always)]
                fn size(&self) -> usize {
                    self.size
                }
            }

            pub(super) mod lengths {
                use super::*;

                #(
                    #[inline(always)]
                    #[allow(unused_variables)]
                    pub(in super::super) const fn #len_ident(#len_fn_arg) -> usize {
                        #len_expr
                    }
                )*
            }

            #tyvis_inner struct Init< #(#varlen_init_ty_param,)* > {
                #(
                    #all_attr
                    #all_vis_inner #all_ident: #all_init_ty,
                )*
            }

            #tyvis_inner struct Muts<'a #(, #generics_structdef)*> {
                #(
                    #all_attr
                    #all_vis_inner #all_ident: #all_mut_ty,
                )*
            }

            #tyvis_inner struct Refs<'a #(, #generics_structdef)*> {
                #(
                    #all_attr
                    #all_vis_inner #all_ident: #all_ref_ty,
                )*
            }

            #tyvis_inner struct LayoutControllers {
                #(
                    #lengths_attr
                    #lengths_vis_inner #lengths_ident: #lengths_ty,
                )*
            }
        }

        #ty_attrs
        #(#d_attrs)*
        #tyvis struct #tyname<#(#generics_structdef),*> #generics_where {
            #(
                #all_attr
                #all_vis #all_ident: #all_decl_ty,
            )*
        }

        impl<#(#generics_impl),*> ::core::ops::Drop for #tyname<#(#generics_apply),*> #generics_where {
            fn drop(&mut self) {
                ::varlen::macro_support::invalid_drop_call()
            }
        }

        unsafe impl<#(#generics_impl),*> ::varlen::VarLen for #tyname<#(#generics_apply),*> #generics_where {
            type Layout = #mod_name::VarLenLayout<#(#layout_generics_apply),*>;

            #[inline(always)]
            fn calculate_layout(&self) -> #mod_name::VarLenLayout<#(#layout_generics_apply),*> {
                let lengths = (
                    #(
                        &self.#lengths_ident,
                    )*
                );
                let size = ::core::mem::size_of::<Self>();
                #(
                    let ((#varlen_ident, #varlen_layout_ident, size)) = #varlen_cat_field_fast;
                )*
                #mod_name::VarLenLayout{
                    #(
                        #varlen_ident,
                        #varlen_layout_ident,
                    )*
                    size,
                }
            }

            const ALIGN: usize = ::varlen::macro_support::array_max(
                &[
                    ::core::mem::align_of::<Self>(),
                    #(
                        #varlen_align_of_field,
                    )*
                ]
            );

            const NEEDS_VDROP: bool =
                #(
                    #all_needs_vdrop ||
                )*
                false;

            unsafe fn vdrop(self: ::core::pin::Pin<&mut Self>, layout: #mod_name::VarLenLayout<#(#layout_generics_apply),*>) {
                let p_typed = self.get_unchecked_mut() as *mut Self;
                let p = p_typed as *mut u8;
                #(
                    #all_drop_field
                )*
            }
        }

        unsafe impl< #(#generics_impl,)* #( #varlen_init_ty_param: #varlen_init_ty_constraint, )* >
                ::varlen::Initializer<#tyname<#(#generics_apply),*>> 
                for #mod_name::Init< #( #varlen_init_ty_param, )* > 
                #generics_where {
            #[inline(always)]
            fn calculate_layout_cautious(&self) -> ::core::option::Option<#mod_name::VarLenLayout<#(#layout_generics_apply),*>> {
                let lengths = (
                    #(
                        &self.#lengths_ident,
                    )*
                );
                let size = ::core::mem::size_of::<#tyname>();
                #(
                    let (#varlen_ident, #varlen_layout_ident, size) = #varlen_cat_field_cautious?;
                )*
                ::core::option::Option::Some(#mod_name::VarLenLayout{
                    #(
                        #varlen_ident,
                        #varlen_layout_ident,
                    )*
                    size,
                })
            }

            #[inline(always)]
            unsafe fn initialize(self, dst: ::core::ptr::NonNull<#tyname<#(#generics_apply),*>>, layout: #mod_name::VarLenLayout<#(#layout_generics_apply),*>) {
                // Initialize header, then initialize tail in program order.
                let p = dst.cast::<u8>();
                ::core::ptr::write(dst.as_ptr(), #tyname {
                    #(
                        #all_ident: #all_init_field,
                    )*
                });
            }
        }

        impl<#(#generics_impl),*> #tyname<#(#generics_apply),*> #generics_where {
            /// Mutable access to all fields simultaneously, except the header.
            #tyvis fn muts(mut self: ::core::pin::Pin<&mut Self>) -> #mod_name::Muts<'_  #(, #generics_apply)*> {
                let layout = ::varlen::VarLen::calculate_layout(self.as_ref().get_ref());
                unsafe {
                    let mut_ref = self.get_unchecked_mut();
                    let mut_ptr = mut_ref as *mut _;
                    #mod_name::Muts {
                        #(
                            #all_ident: #all_mut_field,
                        )*
                    }
                }
            }

            /// Mutable access to all fields simultaneously, except the header.
            #tyvis fn refs(&self) -> #mod_name::Refs<'_ #(, #generics_apply)*> {
                let layout = ::varlen::VarLen::calculate_layout(self);
                unsafe {
                    #mod_name::Refs {
                        #(
                            #all_ident: #all_ref_field,
                        )*
                    }
                }
            }

            #tyvis fn try_set_layout_controllers(
                mut self: ::core::pin::Pin<&mut Self>, layout_controllers: #mod_name::LayoutControllers
            ) -> ::core::result::Result<(), ::varlen::macro_support::LayoutMismatch> {
                let lengths_before = (
                    #(
                        &self.#lengths_ident,
                    )*
                );
                let lengths_after = (
                    #(
                        &layout_controllers.#lengths_ident,
                    )*
                );
                let valid = true
                    #(
                        & (
                            #mod_name::lengths::#len_ident(lengths_before) ==
                            #mod_name::lengths::#len_ident(lengths_after)
                        )
                    )*;
                if valid {
                    let self_mut = unsafe { self.get_unchecked_mut() };
                    #(
                        self_mut.#lengths_ident = layout_controllers.#lengths_ident;
                    )*
                    ::core::result::Result::Ok(())
                } else {
                    ::core::result::Result::Err(::varlen::macro_support::LayoutMismatch)
                }
            }

            /*
            #(
                #varlen_array_attr
                #[inline(always)]
                #varlen_array_vis fn #varlen_array_ident(&self) -> &[#varlen_array_elem_ty] {
                    let offsets = self.header.offsets();
                    unsafe { ::varlen::macro_support::slice_ref(self, offsets.#varlen_array_ident, offsets.#varlen_array_len_ident) }
                }

                #varlen_array_attr
                #[inline(always)]
                #varlen_array_vis fn #varlen_array_mut_ident(self: ::core::pin::Pin<&mut Self>) -> &mut [#varlen_array_elem_ty] {
                    let offsets = self.as_ref().header.offsets();
                    unsafe { ::varlen::macro_support::slice_mut_ref(self, offsets.#varlen_array_ident, offsets.#varlen_array_len_ident) }
                }

                #varlen_array_attr
                #[inline(always)]
                #varlen_array_vis fn #varlen_array_uninit_ident(self: ::core::pin::Pin<&mut Self>) -> &mut [::core::mem::MaybeUninit<#varlen_array_elem_ty>] {
                    let offsets = self.as_ref().header.offsets();
                    unsafe { ::varlen::macro_support::slice_mut_ref(self, offsets.#varlen_array_ident, offsets.#varlen_array_len_ident) }
                }
            )*
            */
        }
    })
}

#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
enum SimpleVisibility {
    Private = 0,
    Super = 1,
    SuperSuper = 2,
    Crate = 3,
    Public = 4,
}

impl quote::ToTokens for SimpleVisibility {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            SimpleVisibility::Private => {}
            SimpleVisibility::Super => tokens.extend(quote!(pub(super))),
            SimpleVisibility::SuperSuper => tokens.extend(quote!(pub(in super::super))),
            SimpleVisibility::Crate => tokens.extend(quote!(pub(crate))),
            SimpleVisibility::Public => tokens.extend(quote!(pub)),
        }
    }
}

impl SimpleVisibility {
    fn bad_visibility(span: proc_macro2::Span) -> Error {
        Error(
            "Visibility must be one of: (none), pub, pub(crate), pub(self), pub(super)",
            span,
        )
    }

    fn try_parse(v: &Visibility) -> Result<(SimpleVisibility, SimpleVisibility), Error> {
        Ok(match v {
            Visibility::Public(_) => (SimpleVisibility::Public, SimpleVisibility::Public),
            Visibility::Crate(_) => (SimpleVisibility::Crate, SimpleVisibility::Crate),
            Visibility::Restricted(r) => {
                if r.path.is_ident("crate") {
                    (SimpleVisibility::Crate, SimpleVisibility::Crate)
                } else if r.path.is_ident("super") {
                    (SimpleVisibility::Super, SimpleVisibility::SuperSuper)
                } else if r.path.is_ident("self") {
                    (SimpleVisibility::Private, SimpleVisibility::Super)
                } else {
                    return Err(Self::bad_visibility(v.span()));
                }
            }
            Visibility::Inherited => (SimpleVisibility::Private, SimpleVisibility::Super),
        })
    }
}

fn mk_layout_ident(ident: &Ident) -> Ident {
    format_ident!("{}_layout", ident)
}

fn mk_init_ident(ident: &Ident) -> Ident {
    format_ident!("Init{}", ident.to_string().to_case(Case::UpperCamel))
}

#[derive(Clone)]
struct OneMeta {
    /// Attributes on the field declaration.
    attr: TokenStream,
    /// Visibility of the field declaration.
    vis: SimpleVisibility,
    /// Visibility of the field declaration, for one module deeper.
    vis_inner: SimpleVisibility,
    /// Name of the field.
    ident: Ident,
}

impl OneMeta {
    fn parse(f: &Field) -> Result<Self, Error> {
        let ident = f.ident.clone().unwrap();
        let attrs = &f.attrs;
        let attr = quote!(#( #attrs )*);
        let (vis, vis_inner) = SimpleVisibility::try_parse(&f.vis)?;
        Ok(OneMeta {
            attr,
            vis,
            vis_inner,
            ident,
        })
    }
}

#[derive(Clone)]
struct Meta {
    /// Attributes on the field declaration.
    attrs: Vec<TokenStream>,
    /// Visibility of the field declaration.
    vis: Vec<SimpleVisibility>,
    /// Visibility of the field declaration, for one module deeper.
    vis_inner: Vec<SimpleVisibility>,
    /// Name of the field.
    ident: Vec<Ident>,
}

impl Meta {
    fn new() -> Self {
        Meta {
            attrs: Vec::new(),
            vis: Vec::new(),
            vis_inner: Vec::new(),
            ident: Vec::new(),
        }
    }

    fn push(&mut self, one: OneMeta) {
        self.attrs.push(one.attr);
        self.vis.push(one.vis);
        self.vis_inner.push(one.vis_inner);
        self.ident.push(one.ident);
    }
}

struct LengthFields {
    /// Metadata for the field.
    meta: Meta,
    /// Type of the field.
    tys: Vec<Type>,
}

impl LengthFields {
    fn new() -> Self {
        LengthFields {
            meta: Meta::new(),
            tys: Vec::new(),
        }
    }
}

struct GenericRefs {
    /// Type identifiers referenced by a type.
    idents: HashSet<Ident>,
    /// Lifetimes referenced by a type.
    lifetimes: HashSet<Lifetime>,
}

impl GenericRefs {
    fn new() -> Self {
        GenericRefs {
            idents: HashSet::new(),
            lifetimes: HashSet::new(),
        }
    }

    fn add(&mut self, ty: &Type) {
        syn::visit::visit_type(self, ty);
    }
}

impl<'ast> syn::visit::Visit<'ast> for GenericRefs {
    fn visit_type_path(&mut self, node: &'ast syn::TypePath) {
        if node.qself.is_none() {
            if let Some(ident) = node.path.get_ident() {
                self.idents.insert(ident.clone());
                return;
            }
        }
        if let Some(it) = &node.qself {
            self.visit_qself(it);
        }
        self.visit_path(&node.path);
    }

    fn visit_lifetime(&mut self, node: &'ast Lifetime) {
        self.lifetimes.insert(node.clone());
    }
}

/// Fields associated with variable-length (array or VarLen) types.
struct FieldMarkers {
    /// Universal metadata
    meta: Meta,
    /// Type parameter for this field's initializer
    init_ty_params: Vec<Ident>,
    /// Constraints on the initializer type.
    init_ty_constraints: Vec<TokenStream>,
    /// 'field_name_#layout'
    layout_idents: Vec<Ident>,
    /// type of 'field_name_#layout'
    layout_ty: Vec<TokenStream>,
    /// Generic types referenced by layout_ty
    layout_ty_refs: GenericRefs,
    /// calls 'cat_field_cautious' with 'size: usize', returning 'Option<(usize, usize, field::Layout)>'
    cat_field_cautious: Vec<TokenStream>,
    /// calls 'cat_field_fast' with 'size: usize', returning 'Option<(usize, usize, field::Layout)>'
    cat_field_fast: Vec<TokenStream>,
    /// Gets the required alignment of this field
    align_of_field: Vec<TokenStream>,
}

impl FieldMarkers {
    fn new() -> Self {
        FieldMarkers {
            meta: Meta::new(),
            init_ty_params: Vec::new(),
            init_ty_constraints: Vec::new(),
            layout_idents: Vec::new(),
            layout_ty: Vec::new(),
            layout_ty_refs: GenericRefs::new(),
            cat_field_cautious: Vec::new(),
            cat_field_fast: Vec::new(),
            align_of_field: Vec::new(),
        }
    }
}

/// All fields, interleaved in program order.
struct AllFields {
    /// Name of the field
    meta: Meta,
    /// Type in the struct declaration
    decl_ty: Vec<TokenStream>,
    /// Type in the Init declaration
    init_ty: Vec<TokenStream>,
    /// Reference type, with lifetime 'a.
    ref_ty: Vec<TokenStream>,
    /// Mutable reference type, with lifetime 'a.
    mut_ty: Vec<TokenStream>,
    /// Writes to the tail if necessary, then evaluates to the initializer for this field.
    init_field: Vec<TokenStream>,
    /// creates a reference to this field, given that '&self, layout: Layout' is in scope
    ref_field: Vec<TokenStream>,
    /// creates a mutable reference to this field, given that '&mut self, layout: Layout' is in scope
    mut_field: Vec<TokenStream>,
    /// calls the drop or vdrop function on this field
    drop_field: Vec<TokenStream>,
    /// Does this field need vdrop?
    needs_vdrop: Vec<TokenStream>,
}

impl AllFields {
    fn new() -> Self {
        AllFields {
            meta: Meta::new(),
            decl_ty: Vec::new(),
            init_ty: Vec::new(),
            ref_ty: Vec::new(),
            mut_ty: Vec::new(),
            init_field: Vec::new(),
            ref_field: Vec::new(),
            mut_field: Vec::new(),
            drop_field: Vec::new(),
            needs_vdrop: Vec::new(),
        }
    }
}

struct LengthExprs {
    len_ident: Vec<Ident>,
    len_expr: Vec<TokenStream>,
}

impl LengthExprs {
    fn new() -> LengthExprs {
        LengthExprs {
            len_ident: Vec::new(),
            len_expr: Vec::new(),
        }
    }
}

struct FieldGroups {
    lengths: LengthFields,
    len_exprs: LengthExprs,
    varlen_fields: FieldMarkers,
    all_fields: AllFields,
}

impl FieldGroups {
    fn new() -> Self {
        FieldGroups {
            lengths: LengthFields::new(),
            len_exprs: LengthExprs::new(),
            varlen_fields: FieldMarkers::new(),
            all_fields: AllFields::new(),
        }
    }

    fn push_normal(&mut self, f: Field) -> Result<(), Error> {
        let meta = OneMeta::parse(&f)?;
        let ident = f.ident.unwrap();
        let span = f.ty.span();
        let ty = f.ty;

        self.all_fields.meta.push(meta);
        self.all_fields.decl_ty.push(quote_spanned! { span => #ty });
        self.all_fields.init_ty.push(quote_spanned! { span => #ty });
        self.all_fields
            .ref_ty
            .push(quote_spanned! { span => &'a #ty });
        self.all_fields
            .mut_ty
            .push(quote_spanned! { span => &'a mut #ty });
        self.all_fields
            .init_field
            .push(quote_spanned! { span => self.#ident });
        self.all_fields
            .ref_field
            .push(quote_spanned! { span => &self.#ident });
        self.all_fields
            .mut_field
            .push(quote_spanned! { span => &mut mut_ref.#ident });
        self.all_fields.drop_field.push(quote_spanned! {span =>
            ::core::ptr::drop_in_place(::core::ptr::addr_of_mut!((*p_typed).#ident));
        });
        self.all_fields.needs_vdrop.push(quote_spanned! {span =>
            ::core::mem::needs_drop::<#ty>()
        });
        Ok(())
    }

    fn push_length(&mut self, f: Field) -> Result<(), Error> {
        let meta = OneMeta::parse(&f)?;
        let ident = f.ident.unwrap();
        let span = f.ty.span();
        let ty = f.ty;

        self.lengths.meta.push(meta.clone());
        self.lengths.tys.push(ty.clone());

        self.all_fields.meta.push(meta);
        self.all_fields.decl_ty.push(quote_spanned! { span => #ty });
        self.all_fields.init_ty.push(quote_spanned! { span => #ty });
        self.all_fields
            .ref_ty
            .push(quote_spanned! { span => &'a #ty });
        self.all_fields
            .mut_ty
            .push(quote_spanned! { span => &'a #ty });
        self.all_fields
            .init_field
            .push(quote_spanned! { span => self.#ident });
        self.all_fields
            .ref_field
            .push(quote_spanned! { span => &self.#ident });
        self.all_fields
            .mut_field
            .push(quote_spanned! { span => &mut mut_ref.#ident });
        self.all_fields.drop_field.push(quote_spanned! {span =>
            ::core::ptr::drop_in_place(::core::ptr::addr_of_mut!((*p_typed).#ident));
        });
        self.all_fields.needs_vdrop.push(quote_spanned! {span =>
            ::core::mem::needs_drop::<#ty>()
        });
        Ok(())
    }

    fn push_varlen(&mut self, f: Field) -> Result<(), Error> {
        let meta = OneMeta::parse(&f)?;
        let ident = f.ident.unwrap();
        let layout_ident = mk_layout_ident(&ident);
        let init_ident = mk_init_ident(&ident);
        let span = f.ty.span();
        let ty = f.ty;

        self.varlen_fields.meta.push(meta.clone());
        self.varlen_fields.init_ty_params.push(init_ident.clone());
        self.varlen_fields
            .init_ty_constraints
            .push(quote_spanned! { span => ::varlen::Initializer<#ty> });
        self.varlen_fields.layout_idents.push(layout_ident.clone());
        self.varlen_fields
            .layout_ty
            .push(quote_spanned! { span => <#ty as ::varlen::VarLen>::Layout });
        self.varlen_fields.layout_ty_refs.add(&ty);
        self.varlen_fields
            .cat_field_cautious
            .push(quote_spanned! { span =>
                ::varlen::macro_support::cat_field_cautious::<#ty, _>(&self.#ident, size)
            });
        self.varlen_fields
            .cat_field_fast
            .push(quote_spanned! { span =>
                ::varlen::macro_support::cat_field_fast::<#ty, _>(self, size)
            });
        self.varlen_fields
            .align_of_field
            .push(quote_spanned! { span =>
                <#ty as ::varlen::VarLen>::ALIGN
            });

        self.all_fields.meta.push(meta);
        self.all_fields
            .decl_ty
            .push(quote_spanned! { span =>::varlen::marker::FieldMarker<#ty> });
        self.all_fields
            .init_ty
            .push(quote_spanned! { span => #init_ident });
        self.all_fields
            .ref_ty
            .push(quote_spanned! { span => &'a #ty });
        self.all_fields
            .mut_ty
            .push(quote_spanned! { span => ::core::pin::Pin<&'a mut #ty> });
        self.all_fields.init_field.push(quote_spanned! { span =>
            ::varlen::macro_support::init_field(self.#ident, p, layout.#ident, layout.#layout_ident)
        });
        self.all_fields.ref_field.push(quote_spanned! { span =>
            ::varlen::macro_support::ref_field(self, layout.#ident)
        });
        self.all_fields.mut_field.push(quote_spanned! { span =>
            ::varlen::macro_support::mut_field(mut_ptr, layout.#ident)
        });
        self.all_fields.drop_field.push(quote_spanned! { span =>
            ::varlen::macro_support::vdrop_field::<#ty>(p, layout.#ident, layout.#layout_ident);
        });
        self.all_fields.needs_vdrop.push(quote_spanned! { span =>
            <#ty as ::varlen::VarLen>::NEEDS_VDROP
        });
        Ok(())
    }

    fn push_varlen_array(&mut self, f: Field, mod_name: &Ident) -> Result<(), Error> {
        let meta = OneMeta::parse(&f)?;
        let ident = f.ident.unwrap();
        let len_ident = format_ident!("{}_len", &ident);
        let init_ident = mk_init_ident(&ident);
        let span = f.ty.span();
        let ty = f.ty;

        let (elem_ty, len_expr) = match &ty {
            Type::Array(a) => {
                let len = &a.len;
                ((*a.elem).clone(), quote_spanned! {len.span()=> #len})
            }
            _ => {
                return Err(Error(
                    "Fields annotated with #[varlen_array] must be of array type",
                    span,
                ))
            }
        };
        let len_span = len_expr.span();

        self.len_exprs
            .len_expr
            .push(quote_spanned!(len_span => #len_expr ));
        self.len_exprs.len_ident.push(len_ident.clone());

        self.varlen_fields.meta.push(meta.clone());
        self.varlen_fields.init_ty_params.push(init_ident.clone());
        self.varlen_fields
            .init_ty_constraints
            .push(quote_spanned! { span => ::varlen::array_init::ArrayInitializer<#elem_ty> });
        self.varlen_fields.layout_idents.push(len_ident.clone());
        self.varlen_fields
            .layout_ty
            .push(quote_spanned! { span => usize });
        self.varlen_fields
            .cat_field_cautious
            .push(quote_spanned! { span =>
                ::varlen::macro_support::cat_array_field_cautious::<#elem_ty>(
                    #mod_name::lengths::#len_ident(lengths), size)
            });
        self.varlen_fields
            .cat_field_fast
            .push(quote_spanned! { span =>
                ::varlen::macro_support::cat_array_field_fast::<#elem_ty>(
                    #mod_name::lengths::#len_ident(lengths), size)
            });
        self.varlen_fields
            .align_of_field
            .push(quote_spanned! { span =>
                ::core::mem::align_of::<#elem_ty>()
            });

        self.all_fields.meta.push(meta);
        self.all_fields
            .init_ty
            .push(quote_spanned! { span => #init_ident });
        self.all_fields
            .decl_ty
            .push(quote_spanned! { span => ::varlen::marker::ArrayMarker<#elem_ty> });
        self.all_fields
            .ref_ty
            .push(quote_spanned! { span => &'a [#elem_ty] });
        self.all_fields
            .mut_ty
            .push(quote_spanned! { span => &'a mut [#elem_ty] });
        self.all_fields.init_field.push(quote_spanned! { span =>
            ::varlen::macro_support::init_array(self.#ident, p, layout.#ident, layout.#len_ident)
        });
        self.all_fields.ref_field.push(quote_spanned! { span =>
            ::varlen::macro_support::ref_array(self, layout.#ident, layout.#len_ident)
        });
        self.all_fields.mut_field.push(quote_spanned! { span =>
            ::varlen::macro_support::mut_array(mut_ptr, layout.#ident, layout.#len_ident)
        });
        self.all_fields.drop_field.push(quote_spanned! { span =>
            ::varlen::macro_support::drop_array::<#elem_ty>(p, layout.#ident, layout.#len_ident);
        });
        self.all_fields.needs_vdrop.push(quote_spanned! { span =>
            ::core::mem::needs_drop::<#elem_ty>()
        });
        Ok(())
    }
}

#[derive(PartialEq, Eq)]
enum FieldType {
    Normal,
    Length,
    Varlen,
    VarlenArray,
}

fn parse_fields(fields: Punctuated<Field, Comma>, mod_name: &Ident) -> Result<FieldGroups, Error> {
    let varlen_attr: Attribute = parse_quote! { #[varlen] };
    let varlen_array_attr: Attribute = parse_quote! { #[varlen_array] };
    let layout_attr: Attribute = parse_quote! { #[controls_layout] };

    let mut field_groups = FieldGroups::new();

    for mut f in fields {
        let mut field_type = FieldType::Normal;
        let mut err = None;

        let mut set_type = |t, span| {
            if field_type != FieldType::Normal {
                err = Some(Err(
                    Error("Field must have at most one of #[varlen], #[varlen_array], #[controls_layout] attributes", span)
                ));
            } else {
                field_type = t;
            }
        };

        f.attrs.retain(|attr| {
            if attr == &varlen_array_attr {
                set_type(FieldType::VarlenArray, attr.span());
                false
            } else if attr == &layout_attr {
                set_type(FieldType::Length, attr.span());
                false
            } else if attr == &varlen_attr {
                set_type(FieldType::Varlen, attr.span());
                false
            } else {
                true
            }
        });
        if let Some(err) = err {
            return err;
        }
        match field_type {
            FieldType::Normal => field_groups.push_normal(f)?,
            FieldType::Length => field_groups.push_length(f)?,
            FieldType::Varlen => field_groups.push_varlen(f)?,
            FieldType::VarlenArray => field_groups.push_varlen_array(f, mod_name)?,
        }
    }
    Ok(field_groups)
}



fn parse_generics(generics: &Generics, filter: Option<GenericRefs>) -> (Vec<GenericParam>, Vec<GenericParam>, Vec<TokenStream>, TokenStream) {
    let mut structdef_params = Vec::new();
    let where_clause = &generics.where_clause;
    let mut impl_params = Vec::new();
    let mut apply_params = Vec::new();
    'outer: for param in &generics.params {
        let (structdef_param, apply_param, impl_param) = match param {
            GenericParam::Const(c) => {
                let ident = &c.ident;
                if let Some(filter) = &filter {
                    if !filter.idents.contains(&ident) {
                        continue 'outer;
                    }
                }
                let structdef_param = c.clone();
                let mut apply_param = c.clone();
                apply_param.default = None;
                (GenericParam::Const(structdef_param), quote!( #ident ), GenericParam::Const(apply_param))
            },
            GenericParam::Type(c) => {
                let ident = &c.ident;
                if let Some(filter) = &filter {
                    if !filter.idents.contains(&ident) {
                       continue 'outer;
                    }
                }
                let structdef_param = c.clone();
                let mut apply_param = c.clone();
                apply_param.default = None;
                (GenericParam::Type(structdef_param), quote!( #ident ),
                GenericParam::Type(apply_param))
            },
            GenericParam::Lifetime(c) => {
                let lifetime = c.lifetime.clone();
                if let Some(filter) = &filter {
                    if !filter.lifetimes.contains(&lifetime) {
                        continue 'outer;
                    }
                }
                let structdef_param = c.clone();
                let apply_param = c.clone();
                (GenericParam::Lifetime(structdef_param), quote!( #lifetime ),
                GenericParam::Lifetime(apply_param))
            },
        };
        structdef_params.push(structdef_param);
        apply_params.push(apply_param);
        impl_params.push(impl_param);
    }
    (structdef_params, impl_params, apply_params, quote!( #where_clause ))
}