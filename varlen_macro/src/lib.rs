#![allow(unused_variables)]

use proc_macro2::TokenStream;
use syn::{DeriveInput, Ident, Data, Fields, Field, Attribute, Type, Visibility};
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::spanned::Spanned;
use syn::parse_quote;
use quote::{quote, quote_spanned, format_ident};
use convert_case::{Case, Casing};

#[proc_macro_attribute]
pub fn define_varlen(attrs: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let attrs: proc_macro2::TokenStream = attrs.into();
    let item: proc_macro2::TokenStream = item.into();
    match define_varlen_impl(attrs.clone(), item.clone()) {
        Ok(x) => {
            x
        },
        Err(Error(msg, span)) => {
            let error = quote_spanned!(span => 
                ::core::compile_error!(#msg);
            );
            quote!( 
                #attrs
                #item
                #error
            )
        },
    }.into()
}

struct Error(&'static str, proc_macro2::Span);

fn define_varlen_impl(ty_attrs: TokenStream, d: TokenStream) -> Result<TokenStream, Error> {
    let d: DeriveInput = match syn::parse2(d) {
        Ok(d) => d,
        Err(e) => return Err(Error("define_varlen could not parse this as a struct", e.span())),
    };
    let d_attrs = d.attrs;
    let fields = 
        if let Data::Struct(s) = d.data {
            if let Fields::Named(n) = s.fields {
                n.named
            } else {
                return Err(Error("define_varlen requires named fields", d.ident.span()))
            }
        } else {
            return Err(Error("define_varlen requires a struct", d.ident.span()))
        };

    let (tyvis, tyvis_inner) = SimpleVisibility::try_parse(&d.vis)?;
    let tyname = d.ident;

    let mut mod_name = format_ident!("{}", tyname.to_string().to_case(Case::Snake));
    mod_name.set_span(proc_macro2::Span::call_site());


    let FieldGroups{
        lengths: LengthFields{
            meta: Meta {
                ident: lengths_ident,
                ..
            },
            tys: lengths_ty,
        },
        len_exprs: LengthExprs{
            len_expr,
            len_ident,
        },
        varlen_fields: FieldMarkers{
            meta: Meta{
                ident: varlen_ident,
                ..
            },
            init_ty_params: varlen_init_ty_param,
            init_ty_constraints: varlen_init_ty_constraint,
            layout_idents: varlen_layout_ident,
            layout_ty: varlen_layout_ty,
            cat_field_fast: varlen_cat_field_fast,
            cat_field_cautious: varlen_cat_field_cautious,
            drop_field: varlen_drop_field,
            align_of_field: varlen_align_of_field,
            needs_drop_tail: varlen_needs_drop_tail,
        },
        all_fields: AllFields{
            meta: Meta{
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
        },
    } = parse_fields(fields, &mod_name)?;

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
            #tyvis_inner struct VarLenLayout {
                pub(super) size: usize,
                #(
                    pub(super) #varlen_ident: usize,
                    pub(super) #varlen_layout_ident: #varlen_layout_ty,
                )*
            }

            impl ::varlen::Layout for VarLenLayout {
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

            #tyvis_inner struct Muts<'a> {
                #(
                    #all_attr
                    #all_vis_inner #all_ident: #all_mut_ty,  
                )*
            }

            #tyvis_inner struct Refs<'a> {
                #(
                    #all_attr
                    #all_vis_inner #all_ident: #all_ref_ty,
                )*
            }
        }

        #ty_attrs
        #(#d_attrs)*
        #tyvis struct #tyname {
            #(
                #all_attr
                #all_vis #all_ident: #all_decl_ty,
            )*
        }

        unsafe impl ::varlen::VarLen for #tyname {
            type Layout = #mod_name::VarLenLayout;

            #[inline(always)]
            fn calculate_layout(&self) -> #mod_name::VarLenLayout {
                let lengths = (
                    #(
                        &self.#lengths_ident,
                    )*
                );
                let size = ::core::mem::size_of::<#tyname>();
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

            const NEEDS_DROP_TAIL: bool = 
                #(
                    #varlen_needs_drop_tail ||
                )*
                false;
            
            unsafe fn drop_tail(self: ::core::pin::Pin<&mut Self>, layout: #mod_name::VarLenLayout) {
                let p = self.get_unchecked_mut() as *mut Self as *mut u8;
                #(
                    #varlen_drop_field
                )*
            }
        }

        unsafe impl< #( #varlen_init_ty_param: #varlen_init_ty_constraint, )* >
                ::varlen::Initializer<#tyname> for #mod_name::Init< #( #varlen_init_ty_param, )* > {
            #[inline(always)]
            fn calculate_layout_cautious(&self) -> ::core::option::Option<#mod_name::VarLenLayout> {
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
            unsafe fn initialize(self, dst: ::core::ptr::NonNull<#tyname>, layout: #mod_name::VarLenLayout) {
                // Initialize header, then initialize tail in program order.
                let p = dst.cast::<u8>();
                ::core::ptr::write(dst.as_ptr(), #tyname {
                    #(
                        #all_ident: #all_init_field,
                    )*
                });
            }
        }

        impl #tyname {
            /// Mutable access to all fields simultaneously, except the header.
            #tyvis fn muts(mut self: ::core::pin::Pin<&mut Self>) -> #mod_name::Muts {
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
            #tyvis fn refs(&self) -> #mod_name::Refs {
                let layout = ::varlen::VarLen::calculate_layout(self);
                unsafe {
                    #mod_name::Refs {
                        #(
                            #all_ident: #all_ref_field,
                        )*
                    }
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

// fn prune_generics(d: &Generics) -> Generics {
//     d.clone()
// }

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
            SimpleVisibility::Private => {},
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
            span
        )
    }

    fn try_parse(v: &Visibility) -> Result<(SimpleVisibility, SimpleVisibility), Error> {
        Ok(
            match v {
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
                        return Err(Self::bad_visibility(v.span()))
                    }
                },
                Visibility::Inherited => (SimpleVisibility::Private, SimpleVisibility::Super),
            }
        )
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
        Ok(OneMeta{
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
        Meta{
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
        LengthFields{
            meta: Meta::new(),
            tys: Vec::new(),
        }
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
    /// calls 'cat_field_cautious' with 'size: usize', returning 'Option<(usize, usize, field::Layout)>'
    cat_field_cautious: Vec<TokenStream>,
    /// calls 'cat_field_fast' with 'size: usize', returning 'Option<(usize, usize, field::Layout)>'
    cat_field_fast: Vec<TokenStream>,
    /// calls the drop_tail function on this field
    drop_field: Vec<TokenStream>,
    /// Gets the required alignment of this field
    align_of_field: Vec<TokenStream>,
    /// Does this field need drop_tail?
    needs_drop_tail: Vec<TokenStream>,
}

impl FieldMarkers {
    fn new() -> Self {
        FieldMarkers{
            meta: Meta::new(),
            init_ty_params: Vec::new(),
            init_ty_constraints: Vec::new(),
            layout_idents: Vec::new(),
            layout_ty: Vec::new(),
            cat_field_cautious: Vec::new(),
            cat_field_fast: Vec::new(),
            drop_field: Vec::new(),
            align_of_field: Vec::new(),
            needs_drop_tail: Vec::new(),
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
        self.all_fields.ref_ty.push(quote_spanned!{ span => &'a #ty });
        self.all_fields.mut_ty.push(quote_spanned!{ span => &'a mut #ty });
        self.all_fields.init_field.push(quote_spanned!{ span => self.#ident });
        self.all_fields.ref_field.push(quote_spanned!{ span => &self.#ident });
        self.all_fields.mut_field.push(quote_spanned!{ span => &mut mut_ref.#ident });
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
        self.all_fields.ref_ty.push(quote_spanned!{ span => &'a #ty });
        self.all_fields.mut_ty.push(quote_spanned!{ span => &'a #ty });
        self.all_fields.init_field.push(quote_spanned!{ span => self.#ident });
        self.all_fields.ref_field.push(quote_spanned!{ span => &self.#ident });
        self.all_fields.mut_field.push(quote_spanned!{ span => &mut_ref.#ident });
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
        self.varlen_fields.init_ty_constraints.push(quote_spanned! { span => ::varlen::Initializer<#ty> });
        self.varlen_fields.layout_idents.push(layout_ident.clone());
        self.varlen_fields.layout_ty.push(quote_spanned! { span => <#ty as ::varlen::VarLen>::Layout });
        self.varlen_fields.cat_field_cautious.push(quote_spanned! { span =>
            ::varlen::macro_support::cat_field_cautious::<#ty, _>(&self.#ident, size)
        });
        self.varlen_fields.cat_field_fast.push(quote_spanned! { span =>
            ::varlen::macro_support::cat_field_fast::<#ty, _>(&self, size)
        });
        self.varlen_fields.drop_field.push(quote_spanned! { span =>
            ::varlen::macro_support::drop_field::<#ty>(p, layout.#ident, layout.#layout_ident);
        });
        self.varlen_fields.align_of_field.push(quote_spanned! { span =>
            <#ty as ::varlen::VarLen>::ALIGN
        });
        self.varlen_fields.needs_drop_tail.push(quote_spanned! { span =>
            (<#ty as ::varlen::VarLen>::NEEDS_DROP_TAIL || ::core::mem::needs_drop::<#ty>())
        });

        self.all_fields.meta.push(meta);
        self.all_fields.decl_ty.push(quote_spanned! { span =>::varlen::marker::FieldMarker<#ty> });
        self.all_fields.init_ty.push(quote_spanned! { span => #init_ident });
        self.all_fields.ref_ty.push(quote_spanned!{ span => &'a #ty });
        self.all_fields.mut_ty.push(quote_spanned!{ span => ::core::pin::Pin<&'a mut #ty> });
        self.all_fields.init_field.push(quote_spanned! { span =>
            ::varlen::macro_support::init_field(self.#ident, p, layout.#ident, layout.#layout_ident)
        });
        self.all_fields.ref_field.push(quote_spanned!{ span => 
            ::varlen::macro_support::ref_field(self, layout.#ident)
        });
        self.all_fields.mut_field.push(quote_spanned!{ span => 
            ::varlen::macro_support::mut_field(mut_ptr, layout.#ident)
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
                ((*a.elem).clone(), quote_spanned!{len.span()=> #len})
            },
            _ => 
                return Err(Error(
                    "Fields annotated with #[varlen_array] must be of array type",
                    span)),
        };
        let len_span = len_expr.span();

        self.len_exprs.len_expr.push(quote_spanned!(len_span => #len_expr ));
        self.len_exprs.len_ident.push(len_ident.clone());

        self.varlen_fields.meta.push(meta.clone());
        self.varlen_fields.init_ty_params.push(init_ident.clone());
        self.varlen_fields.init_ty_constraints.push(quote_spanned! { span => ::varlen::ArrayInitializer<#elem_ty> });
        self.varlen_fields.layout_idents.push(len_ident.clone());
        self.varlen_fields.layout_ty.push(quote_spanned! { span => usize });
        self.varlen_fields.cat_field_cautious.push(quote_spanned! { span =>
            ::varlen::macro_support::cat_array_field_cautious::<#elem_ty>(
                #mod_name::lengths::#len_ident(lengths), size)
        });
        self.varlen_fields.cat_field_fast.push(quote_spanned! { span =>
            ::varlen::macro_support::cat_array_field_fast::<#elem_ty>(
                #mod_name::lengths::#len_ident(lengths), size)
        });
        self.varlen_fields.drop_field.push(quote_spanned! { span =>
            ::varlen::macro_support::drop_array::<#elem_ty>(p, layout.#ident, layout.#len_ident);        
        });
        self.varlen_fields.align_of_field.push(quote_spanned! { span =>
            ::core::mem::align_of::<#elem_ty>()
        });
        self.varlen_fields.needs_drop_tail.push(quote_spanned! { span =>
            ::core::mem::needs_drop::<#elem_ty>()
        });

        self.all_fields.meta.push(meta);
        self.all_fields.init_ty.push(quote_spanned! { span => #init_ident });
        self.all_fields.decl_ty.push(quote_spanned! { span => ::varlen::marker::ArrayMarker<#elem_ty> });
        self.all_fields.ref_ty.push(quote_spanned!{ span => &'a [#elem_ty] });
        self.all_fields.mut_ty.push(quote_spanned!{ span => &'a mut [#elem_ty] });
        self.all_fields.init_field.push(quote_spanned! { span =>
            ::varlen::macro_support::init_array(self.#ident, p, layout.#ident, layout.#len_ident)
        });
        self.all_fields.ref_field.push(quote_spanned!{ span => 
            ::varlen::macro_support::ref_array(self, layout.#ident, layout.#len_ident)
        });
        self.all_fields.mut_field.push(quote_spanned!{ span => 
            ::varlen::macro_support::mut_array(mut_ptr, layout.#ident, layout.#len_ident)
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
    let varlen_attr: Attribute = parse_quote!{ #[varlen] };
    let varlen_array_attr: Attribute = parse_quote!{ #[varlen_array] };
    let layout_attr: Attribute = parse_quote!{ #[controls_layout] };

    let mut field_groups = FieldGroups::new();

    for mut f in fields {
        let mut field_type = FieldType::Normal;
        let mut err = None;

        let mut set_type = |t, span| {
            if field_type != FieldType::Normal {
                err = Some(Err(
                    Error("Field must have at most one of #[varlen], #[varlen_array], #[length] attributes", span)
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
