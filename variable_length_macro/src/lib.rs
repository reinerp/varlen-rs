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

    let FieldGroups{
        header_fields: NormalFields{
            attrs: header_attr,
            vis: header_vis,
            vis_inner: header_vis_inner,
            idents: header_ident,
            tys: header_ty,
        },
        mut_fields: NormalFields{
            attrs: mut_attr,
            vis: mut_vis,
            vis_inner: mut_vis_inner,
            idents: mut_ident,
            tys: mut_ty,
        },
        varlen_fields: VarLenFields{
            attrs: varlen_attr,
            vis: varlen_vis,
            vis_inner: varlen_vis_inner,
            idents: varlen_ident,
            mut_idents: varlen_mut_ident,
            uninit_idents: varlen_uninit_ident,
            len_idents: varlen_len_ident,
            elem_tys: varlen_elem_ty,
            len_fns: varlen_len_fn,
            ty_params: varlen_ty_param    
        },
    } = parse_fields(fields)?;

    let headerty_vis = header_vis.iter().copied().max().unwrap_or(tyvis);
    let headerty_vis_inner = header_vis_inner.iter().copied().max().unwrap_or(tyvis_inner);

    let mutty_vis = mut_vis.iter().copied().max().unwrap_or(tyvis);
    let mutty_vis_inner = mut_vis_inner.iter().copied().max().unwrap_or(tyvis_inner);

    let mutref_vis = header_vis.iter().chain(mut_vis.iter()).chain(varlen_vis.iter()).copied().max().unwrap_or(tyvis);
    let mutref_vis_inner = header_vis_inner.iter().chain(mut_vis_inner.iter()).chain(varlen_vis_inner.iter()).copied().max().unwrap_or(tyvis_inner);

    // let (varlen_fields, normal_fields): (Vec<Field>, _) = 
    //     fields.into_iter().partition(|p| has_varlen(p));
    // let (header_fields, mut_fields): (Vec<Field>, _) =
    //     normal_fields.into_iter().partition(|p| has_header(p));
    
    // let NormalFields{attrs: header_attr, vis: header_vis, idents: header_ident, tys: header_ty} = parse_normal_fields(&header_fields);
    // let headerty_vis =  max_visibility(header_vis.iter());
    // let headerty_vis_inner = lift_field_vis(&headerty_vis);

    // let VarLenFields{
    // } = parse_varlen_fields(varlen_fields);
    // let (varlen_names, varlen_elem_tys, varlen_lens, varlen_attrs)
    // let varlen_names: Vec<_> = varlen_fields.iter().map(|f| f.ident.as_ref().unwrap()).collect();
    // let varlen_elem_tys: Vec<_> = varlen_fields.iter().map(|f| f.ty)

    // let generics = d.generics;
    // let header_generics = prune_generics(&generics);
    

    let mut mod_name = format_ident!("{}", tyname.to_string().to_case(Case::Snake));
    mod_name.set_span(proc_macro2::Span::call_site());

    let mut_ty_decl = quote! {
        /// Mutable fields.
        /// 
        /// These are fixed-size fields that cannot influence the trailing array lengths.
        /// As a result, these may be freely mutated.
        #mutty_vis_inner struct MutFields {
            #(
                #mut_attr
                #mut_vis_inner #mut_ident: #mut_ty,
            )*
        }
    };

    // Indicator type to use as an "if" condition for disabling the 'MutFields' struct.
    let has_mut_ty = if mut_ident.len() > 0 {
        vec![quote!()]
    } else {
        vec![]
    };

    Ok(quote! {
        #tyvis mod #mod_name {
            use super::*;

            /// Array offsets and lengths for all trailing arrays.
            pub(super) struct Offsets {
                #(
                    pub(super) #varlen_ident: usize,
                    pub(super) #varlen_len_ident: usize,
                )*
            }

            /// Header fields.
            /// 
            /// These are the fields that can influence the trailing array lengths, and which
            /// therefore must remain constant once the array is allocated.
            #headerty_vis_inner struct Header {
                #(
                    #header_attr
                    #header_vis_inner #header_ident: #header_ty,
                )*
            }

            impl Header {
                #(
                    #varlen_len_fn
                )*

                #[inline]
                pub(super) const fn size_cautious(&self) -> ::core::option::Option<usize> {
                    let size = ::core::option::Option::Some(::core::mem::size_of::<super::#tyname>());
                    #(
                        let size = ::variable_length::macro_support::cat_array_cautious::<#varlen_elem_ty>(size, self.#varlen_len_ident());
                    )*
                    size
                }

                #[inline]
                pub(super) const unsafe fn size_fast(&self) -> usize {
                    let size = ::core::mem::size_of::<super::#tyname>();
                    #(
                        let size = ::variable_length::macro_support::cat_array_fast::<#varlen_elem_ty>(size, self.#varlen_len_ident());
                    )*
                    size
                }

                #[inline(always)]
                pub(super) const fn offsets(&self) -> Offsets {
                    let offset = ::core::mem::size_of::<super::#tyname>();
                    // Initialize varlen fields first, because they require references to the header fields.
                    #(
                        let offset = ::variable_length::macro_support::round_array_fast::<#varlen_elem_ty>(offset);
                        let #varlen_ident = offset;
                        let #varlen_len_ident = self.#varlen_len_ident();
                        let offset = offset.wrapping_add(#varlen_len_ident);
                    )*
                    Offsets{
                        #( #varlen_ident, #varlen_len_ident, )*
                    }
                }
            }

            #(
                #has_mut_ty // Disable if there's no MutFields type
                #mut_ty_decl
            )*

            #tyvis_inner struct Init< #(#varlen_ty_param,)* > {
                #headerty_vis_inner header: Header,
                #(
                    #has_mut_ty
                    #mutty_vis_inner mut_fields: MutFields,
                )*
                #(
                    #varlen_attr
                    #varlen_vis_inner #varlen_ident: #varlen_ty_param,
                )*
            }

            #mutref_vis_inner struct MutRef<'a> {
                #headerty_vis_inner header: &'a Header,
                #(
                    #has_mut_ty
                    #mutty_vis_inner mut_fields: &'a mut MutFields,
                )*
                #(
                    #varlen_attr
                    #varlen_vis_inner #varlen_ident: &'a mut [#varlen_elem_ty],
                )*
            }

            /// Implementation for dropping the tail.
            #tyvis_inner struct DropTailFn(pub(super) Offsets);
            
            impl ::variable_length::DropTailFn<super::#tyname> for DropTailFn {
                unsafe fn drop_tail(self, mut p: ::core::pin::Pin<&mut super::#tyname>) {
                    #(
                        ::core::ptr::drop_in_place(
                            ::variable_length::macro_support::slice_mut_ref(p.as_mut(), self.0.#varlen_ident, self.0.#varlen_len_ident) as *mut [#varlen_elem_ty]
                        );
                    )*
                }
            }
        }

        #ty_attrs
        #(#d_attrs)*
        #tyvis struct #tyname {
            #headerty_vis header: #mod_name::Header,
            #(
                #has_mut_ty
                #mutty_vis mut_fields: #mod_name::MutFields,
            )*
            #(
                #varlen_attr
                #varlen_vis #varlen_ident: ::variable_length::VarLenField<[#varlen_elem_ty]>,
            )*
        }

        unsafe impl ::variable_length::VarLen for #tyname {
            // TODO(reinerp): Need a strategy to avoid things like core::mem::size_of_val(self),
            // which would be different if 'self' takes different types. That could lead to unsafety.
            //
            // Possible solutions:
            //  * always evaluate layout on the same type.
            //    * could be 'header'. Need a way to construct header from Init type and main type.
            //  * could evaluate without 'self' in scope. Reasonable idea! Instead, just have
            //    field names in scope, as local variables. Actually I'm starting to really like this...
            
            #[inline]
            fn size(&self) -> usize {
                unsafe { self.header.size_fast() }
            }

            const ALIGN: usize = ::variable_length::macro_support::array_max(
                &[
                    ::core::mem::align_of::<Self>(),
                    #(
                        ::core::mem::align_of::<#varlen_elem_ty>(),
                    )*
                ]
            );

            const NEEDS_DROP_TAIL: bool = 
                #(
                    ::core::mem::needs_drop::<#varlen_elem_ty>() ||
                )*
                false;
            
            type DropTailFn = #mod_name::DropTailFn;

            #[inline]
            fn prepare_drop_tail(&self) -> Self::DropTailFn {
                #mod_name::DropTailFn(self.header.offsets())
            }
        }

        unsafe impl< #( #varlen_ty_param: ::variable_length::ArrayInitializer<#varlen_elem_ty>, )* 
        > ::variable_length::VarLenInitializer<#tyname> for #mod_name::Init< #( #varlen_ty_param, )* > {
            unsafe fn initialize(self, dst: ::core::ptr::NonNull<#tyname>) {
                let offsets = self.header.offsets();
                let p = dst.cast::<u8>();
                // Initialize varlen fields first, because they require references to the header fields.
                #(
                    self.#varlen_ident.initialize(::variable_length::macro_support::slice_ptr::<#varlen_elem_ty>(
                        p, offsets.#varlen_ident, offsets.#varlen_len_ident));
                )*

                // Initialize header fields, consuming them.
                let written_header = #tyname {
                    header: self.header,
                    #(
                        #has_mut_ty
                        mut_fields: self.mut_fields,
                    )*
                    #(
                        #varlen_ident: ::variable_length::VarLenField::new_unchecked(),
                    )*
                };
                ::core::ptr::write(dst.as_ptr(), written_header);
            }

            #[inline]
            fn required_size(&self) -> ::core::option::Option<usize> {
                self.header.size_cautious()
            }
        }

        impl #tyname {
            /// Mutable access to all fields simultaneously, except the header.
            #mutref_vis fn mut_ref(mut self: ::core::pin::Pin<&mut Self>) -> #mod_name::MutRef {
                let offsets = self.header.offsets();
                unsafe {
                    #(
                        let #varlen_ident = ::variable_length::macro_support::slice_mut_ref_split(self.as_mut(), offsets.#varlen_ident, offsets.#varlen_len_ident);
                    )*
                    let m = self.get_unchecked_mut();
                    #mod_name::MutRef {
                        header: &m.header,
                        #(
                            #has_mut_ty
                            mut_fields: &mut m.mut_fields,
                        )*
                        #(
                            #varlen_ident,
                        )*
                    }
                }
            }

            #(
                #has_mut_ty
                /// Gets a mutable reference to the mutable fields.
                #mutty_vis fn mut_fields_mut(self: ::core::pin::Pin<&mut Self>) -> &mut #mod_name::MutFields {
                    unsafe {
                        &mut self.get_unchecked_mut().mut_fields
                    }
                }
            )*

            #(
                #varlen_attr
                #[inline(always)]
                #varlen_vis fn #varlen_ident(&self) -> &[#varlen_elem_ty] {
                    let offsets = self.header.offsets();
                    unsafe { ::variable_length::macro_support::slice_ref(self, offsets.#varlen_ident, offsets.#varlen_len_ident) }
                }

                #varlen_attr
                #[inline(always)]
                #varlen_vis fn #varlen_mut_ident(self: ::core::pin::Pin<&mut Self>) -> &mut [#varlen_elem_ty] {
                    let offsets = self.as_ref().header.offsets();
                    unsafe { ::variable_length::macro_support::slice_mut_ref(self, offsets.#varlen_ident, offsets.#varlen_len_ident) }
                }

                #varlen_attr
                #[inline(always)]
                #varlen_vis fn #varlen_uninit_ident(self: ::core::pin::Pin<&mut Self>) -> &mut [::core::mem::MaybeUninit<#varlen_elem_ty>] {
                    let offsets = self.as_ref().header.offsets();
                    unsafe { ::variable_length::macro_support::slice_mut_ref(self, offsets.#varlen_ident, offsets.#varlen_len_ident) }
                }
            )*
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

    // fn inner(self, span: proc_macro2::Span) -> Result<SimpleVisibility, Error> {
    //     Ok(
    //         match self {
    //             SimpleVisibility::Private => SimpleVisibility::Super,
    //             SimpleVisibility::Super => SimpleVisibility::SuperSuper,
    //             SimpleVisibility::SuperSuper => return Err(Self::bad_visibility(span)),
    //             SimpleVisibility::Crate => SimpleVisibility::Crate,
    //             SimpleVisibility::Public => SimpleVisibility::Public,
    //         }
    //     )
    // }
}

struct NormalFields {
    attrs: Vec<TokenStream>,
    vis: Vec<SimpleVisibility>,
    vis_inner: Vec<SimpleVisibility>,
    idents: Vec<Ident>,
    tys: Vec<Type>,
}

impl NormalFields {
    fn new() -> Self {
        NormalFields{
            attrs: Vec::new(),
            vis: Vec::new(),
            vis_inner: Vec::new(),
            idents: Vec::new(),
            tys: Vec::new(),
        }
    }

    fn push(&mut self, f: Field) -> Result<(), Error> {
        let attrs = f.attrs;
        self.attrs.push(quote!(#( #attrs )*));
        let (vis, vis_inner) = SimpleVisibility::try_parse(&f.vis)?;
        self.vis.push(vis);
        self.vis_inner.push(vis_inner);
        self.idents.push(f.ident.unwrap());
        self.tys.push(f.ty);
        Ok(())
    }
}

struct VarLenFields {
    attrs: Vec<TokenStream>,
    vis: Vec<SimpleVisibility>,
    vis_inner: Vec<SimpleVisibility>,
    idents: Vec<Ident>,
    mut_idents: Vec<Ident>,
    uninit_idents: Vec<Ident>,
    len_idents: Vec<Ident>,
    elem_tys: Vec<Type>,
    len_fns: Vec<TokenStream>,
    ty_params: Vec<Ident>,
}

impl VarLenFields {
    fn new() -> Self {
        VarLenFields{
            attrs: Vec::new(),
            vis: Vec::new(),
            vis_inner: Vec::new(),
            idents: Vec::new(),
            mut_idents: Vec::new(),
            uninit_idents: Vec::new(),
            len_idents: Vec::new(),
            elem_tys: Vec::new(),
            len_fns: Vec::new(),
            ty_params: Vec::new(),
        }
    }

    fn push(&mut self, f: Field) -> Result<(), Error> {
        let span = f.span();
        let attrs = f.attrs;
        self.attrs.push(quote_spanned!{span=> #(#attrs)* });
        let (vis, vis_inner) = SimpleVisibility::try_parse(&f.vis)?;
        self.vis.push(vis);
        self.vis_inner.push(vis_inner);
        self.mut_idents.push(format_ident!("{}_mut", f.ident.as_ref().unwrap()));
        self.uninit_idents.push(format_ident!("{}_uninit", f.ident.as_ref().unwrap()));
        let len_ident = format_ident!("{}_len", f.ident.as_ref().unwrap());
        self.ty_params.push(format_ident!("Init{}", f.ident.as_ref().unwrap().to_string().to_case(Case::UpperCamel)));
        self.idents.push(f.ident.unwrap());
        let (elem_ty, len_expr) = match f.ty {
            Type::Array(a) => {
                let len = a.len;
                (*a.elem, quote_spanned!{len.span()=> #len})
            },
            _ => 
                return Err(Error(
                    "Fields annotated with #[varlen] must be of array type",
                    f.ty.span())),
        };
        self.elem_tys.push(elem_ty);
        let len_span = len_expr.span();
        self.len_fns.push(
            quote_spanned!(len_span=>
                #[inline(always)]
                pub(super) const fn #len_ident(&self) -> usize {
                    #len_expr
                }
            ));
        self.len_idents.push(len_ident);

        Ok(())
    }
}

struct FieldGroups {
    header_fields: NormalFields,
    mut_fields: NormalFields,
    varlen_fields: VarLenFields,
}

fn parse_fields(fields: Punctuated<Field, Comma>) -> Result<FieldGroups, Error> {
    let varlen_attr: Attribute = parse_quote!{ #[varlen] };
    let header_attr: Attribute = parse_quote!{ #[header] };

    let mut header_fields = NormalFields::new();
    let mut mut_fields = NormalFields::new();
    let mut varlen_fields = VarLenFields::new();
    for mut f in fields {
        let mut varlen_span = None;
        let mut header_span = None;
        f.attrs.retain(|attr| {
            if attr == &varlen_attr {
                varlen_span = Some(attr.span());
                false
            } else if attr == &header_attr {
                header_span = Some(attr.span());
                false
            } else {
                true
            }
        });
        match (varlen_span, header_span) {
            (Some(varlen_span), Some(_)) => return Err(
                Error("Field must have at most one of #[varlen] and #[header] attributes", varlen_span)),
            (None, Some(_)) => header_fields.push(f)?,
            (Some(_), None) => varlen_fields.push(f)?,
            (None, None) => mut_fields.push(f)?,
        }
    }
    Ok(FieldGroups{header_fields, mut_fields, varlen_fields})
}

// fn parse_varlen_fields(varlen_fields: Punctuated<Field, Comma>) -> VarLenFields {
//     let varlen_attr: Attribute = parse_quote!{ #[varlen] };
//     let mut attrs = Vec::new();
//     let mut vises = Vec::new();
//     let mut idents = Vec::new();
//     let mut len_idents = Vec::new();
//     let mut uninit_idents = Vec::new();
//     let mut mut_idents = Vec::new();
//     let mut elem_tys = Vec::new();
//     let mut len_exprs = Vec::new();
//     let mut ty_params = Vec::new();
//     // let mut varlen_marker_fields = Vec::new();
//     // let mut varlen_init_ty_constraints = Vec::new();
//     // let mut varlen_init_tys = Vec::new();
//     // let mut varlen_init_fields = Vec::new();
//     for (i, f) in varlen_fields.into_iter().enumerate() {
//     }
//     VarLenFields{attrs, vises, idents, mut_idents, uninit_idents, len_idents, elem_tys, len_exprs, ty_params}
// }

// fn max_visibility<'t>(b: impl Iterator<Item= &'t Visibility>) -> Visibility {
//     let mut r = Visibility::Inherited;
//     for v in b {
//         if lt_visibility(&r, v) {
//             r = v.clone();
//         }
//     }
//     r
// }

// fn lt_visibility(a: &Visibility, b: &Visibility) -> bool {
//     vis_rank(a) < vis_rank(b)
// }

// fn vis_rank(a: &Visibility) -> usize {
//     match a {
//         Visibility::Public(_) => 3,
//         Visibility::Crate(_) => 2,
//         Visibility::Restricted(r) => {
//             if r.path.is_ident("crate") {
//                 2
//             } else if r.path.is_ident("super") {
//                 1
//             } else if r.path.is_ident("self") {
//                 0
//             } else {
//                 panic!("Unexpected visibility; pub(in foo) is not supported")
//             }
//         },
//         Visibility::Inherited => 0,
//     }
// }

// fn parse_normal_fields(header_fields: &Punctuated<Field, Comma>) -> NormalFields {
//     let mut attrs = Vec::new();
//     let mut idents = Vec::new();
//     let mut tys = Vec::new();
//     let mut vis = Vec::new();
//     for f in header_fields {
//         let f_attrs = &f.attrs;
//         attrs.push(quote_spanned!{f.span() => #( #f_attrs )* });
//         idents.push(f.ident.as_ref().unwrap());
//         tys.push(&f.ty);
//         vis.push(lift_field_vis(&f.vis));
//     }
//     NormalFields{attrs, vis, idents, tys}
// }

// fn lift_field_vis(a: &Visibility) -> Visibility {
//     match a {
//         Visibility::Public(_) => a.clone(),
//         Visibility::Crate(_) => a.clone(),
//         Visibility::Restricted(r) => {
//             if r.path.is_ident("crate") {
//                 a.clone()
//             } else if r.path.is_ident("super") {
//                 parse_quote!{ pub(in super::super) }
//             } else if r.path.is_ident("self") {
//                 parse_quote!{ pub(super) }
//             } else {
//                 panic!("Unexpected visibility; pub(in foo) is not supported")
//             }
//         },
//         Visibility::Inherited => {
//             parse_quote!{ pub(super) }  
//         },
//     }
// }