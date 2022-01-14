use proc_macro2::TokenStream;
use syn::{DeriveInput, Ident, parse_macro_input, Data, Fields, Field, Attribute, Type, Visibility};
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::spanned::Spanned;
use syn::parse_quote;
use quote::{quote, quote_spanned, format_ident};
use convert_case::{Case, Casing};

#[proc_macro_attribute]
pub fn define_varlen(_attrs: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let d: DeriveInput = parse_macro_input!(item);
    let fields = match d.data {
        Data::Struct(s) => match s.fields {
            Fields::Named(n) => n.named,
            Fields::Unnamed(_) => return quote_spanned!{d.ident.span()=>
                ::core::compile_error!("define_varlen! requires structs to have named fields");
            }.into(),
            Fields::Unit => return quote_spanned!{d.ident.span()=>
                ::core::compile_error!("define_varlen! does not support unit types");
            }.into(),
        },
        Data::Enum(e) => return quote_spanned! {e.enum_token.span()=>
            ::core::compile_error!("define_varlen! only supports structs");
        }.into(),
        Data::Union(u) => return quote_spanned! {u.union_token.span()=>
            ::core::compile_error!("define_varlen! only supports structs");
        }.into(),
    };

    let tyvis = d.vis;
    let tyvis_inner = lift_field_vis(&tyvis);
    let tyname = d.ident;
    // let ctor_vis = min_visibility(&tyvis, fields.iter().map(|f| &f.vis)).clone();

    let (varlen_fields, header_fields): (Punctuated<Field, Comma>, _) = 
        fields.into_pairs().partition(|p| has_varlen(p.value()));
    
    let (header_attr, header_vis, header_idents, header_tys) = parse_header_fields(&header_fields);

    let VarLenFields{
        attrs: varlen_attr,
        vises: varlen_vis,
        idents: varlen_ident,
        mut_idents: varlen_mut_ident,
        uninit_idents: varlen_uninit_ident,
        len_idents: varlen_len_ident,
        elem_tys: varlen_elem_ty,
        len_exprs: varlen_len_expr,
        ty_params: varlen_ty_param
    } = parse_varlen_fields(varlen_fields);
    // let (varlen_names, varlen_elem_tys, varlen_lens, varlen_attrs)
    // let varlen_names: Vec<_> = varlen_fields.iter().map(|f| f.ident.as_ref().unwrap()).collect();
    // let varlen_elem_tys: Vec<_> = varlen_fields.iter().map(|f| f.ty)

    // let generics = d.generics;
    // let header_generics = prune_generics(&generics);
    

    let mut pub_mod_name = format_ident!("{}", tyname.to_string().to_case(Case::Snake));
    pub_mod_name.set_span(proc_macro2::Span::call_site());
    // TODO(reinerp): use Span::def_site() once stable.
    let hidden_mod_name = format_ident!("priv_mod_varlen_{}", &pub_mod_name);

    quote! {
        #tyvis mod #pub_mod_name {
            #tyvis_inner struct Header {
                #(
                    #header_attr
                    #header_vis #header_idents: #header_tys,
                )*
            }

            #tyvis_inner struct Init< #(#varlen_ty_param,)* > {
                pub header: Header,
                #(
                    #varlen_attr
                    #varlen_vis #varlen_ident: #varlen_ty_param,
                )*
            }
        }

        mod #hidden_mod_name {
            struct Offsets {
                #(
                    #varlen_ident: usize,
                    #varlen_len_ident: usize,
                )*
            }

            impl super::#pub_mod_name::Header {
                #(
                    #[inline(always)]
                    const fn #varlen_len_ident(&self) -> usize {
                        #varlen_len_expr
                    }
                )*

                #[inline]
                const fn layout_cautious(&self) -> ::core::option::Option<::core::alloc::Layout> {
                    let layout = ::variable_length::macro_support::layout_of::<Self>();
                    #(
                        let layout = ::variable_length::macro_support::cat_array_cautious::<#varlen_elem_ty>(layout, self.#varlen_len_ident());
                    )*
                    ::variable_length::macro_support::to_alloc_layout(layout)
                }

                #[inline]
                const unsafe fn layout_fast(&self) -> ::core::alloc::Layout {
                    let layout = ::variable_length::macro_support::raw_layout_of::<Self>();
                    #(
                        let layout = ::variable_length::macro_support::cat_array_fast::<#varlen_elem_ty>(layout, self.#varlen_len_ident());
                    )*
                    unsafe { ::variable_length::macro_support::to_alloc_layout_unchecked(layout) }
                }

                #[inline(always)]
                const fn offsets(&self) -> Offsets {
                    let offset = ::core::mem::size_of::<#tyname>();
                    // Initialize varlen fields first, because they require references to the header fields.
                    #(
                        let offset = ::variable_length::macro_support::round_array_fast::<#varlen_elem_ty>(offset);
                        let #varlen_ident = offset;
                        let #varlen_len_ident = #varlen_len_expr;
                        let offset = offset.wrapping_add(#varlen_len_ident);
                    )*
                    Offsets{
                        #( #varlen_ident, #varlen_len_ident, )*
                    }
                }
            }

            pub(super) struct DropTailFn(Offsets);
            
            impl ::variable_length::DropTailFn<#tyname> for DropTailFn {
                unsafe fn drop_tail(self, mut p: ::core::pin::Pin<&mut #tyname>) {
                    #(
                        ::core::ptr::drop_in_place(
                            ::variable_length::macro_support::slice_mut_ref(p.as_mut(), self.0.#varlen_ident, self.0.#varlen_len_ident) as *mut [#varlen_elem_ty]
                        );
                    )*
                }
            }

            pub(super) struct #tyname {
                /* private */ header: super::#pub_mod_name::Header,
                #(
                    #varlen_attr
                    /* private */ #varlen_ident: ::core::marker::PhantomData<[#varlen_elem_ty]>,
                )*
                unpin: ::core::marker::PhantomPinned,
            }

            impl ::variable_length::VarLen for #tyname {
                // TODO(reinerp): Need a strategy to avoid things like core::mem::size_of_val(self),
                // which would be different if 'self' takes different types. That could lead to unsafety.
                //
                // Possible solutions:
                //  * always evaluate layout on the same type.
                //    * could be 'header'. Need a way to construct header from Init type and main type.
                //  * could evaluate without 'self' in scope. Reasonable idea! Instead, just have
                //    field names in scope, as local variables. Actually I'm starting to really like this...
                
                #[inline]
                fn layout(&self) -> ::core::alloc::Layout {
                    unsafe { self.header.layout_fast() }
                }

                const NEEDS_DROP_TAIL: bool = 
                    #(
                        ::core::mem::needs_drop::<#varlen_elem_ty>() ||
                    )*
                    false;
                
                type DropTailFn = DropTailFn;

                fn prepare_drop_tail(&self) -> DropTailFn {
                    DropTailFn(self.header().offsets())
                }
            }

            unsafe impl< #( #varlen_ty_param: ::variable_length::Initializer<[#varlen_elem_ty]>, )* 
            > ::variable_length::Initializer<#tyname> for super::#pub_mod_name::Init< #( #varlen_ty_param, )* > {
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
                            #varlen_ident: ::core::marker::PhantomData,
                        )*
                        unpin: ::core::marker::PhantomPinned
                    };
                    ::core::ptr::write(dst.as_ptr(), written_header);
                }
            }

            unsafe impl< #( #varlen_ty_param: ::variable_length::Initializer<[#varlen_elem_ty]>, )* 
            > ::variable_length::SizedInitializer<#tyname> for super::#pub_mod_name::Init< #( #varlen_ty_param, )* > {
                #[inline]
                fn layout(&self) -> ::core::option::Option<::core::alloc::Layout> {
                    self.header.layout_cautious()
                }
            }


            impl #tyname {
                /// Immutable access to header
                #[inline(always)]
                #tyvis_inner fn header(&self) -> &super::#pub_mod_name::Header {
                    &self.header
                }

                #(
                    // Immutable access to varlen fields
                    #varlen_attr
                    #[inline(always)]
                    #varlen_vis fn #varlen_ident(&self) -> &[#varlen_elem_ty] {
                        let offsets = self.header.offsets();
                        unsafe { ::variable_length::macro_support::slice_ref(self, offsets.#varlen_ident, offsets.#varlen_len_ident) }
                    }

                    // Mutable access to varlen fields
                    #varlen_attr
                    #[inline(always)]
                    #varlen_vis fn #varlen_mut_ident(self: ::core::pin::Pin<&mut Self>) -> &mut [#varlen_elem_ty] {
                        let offsets = self.as_ref().header.offsets();
                        unsafe { ::variable_length::macro_support::slice_mut_ref(self, offsets.#varlen_ident, offsets.#varlen_len_ident) }
                    }

                    // Uninitialized access to varlen fields
                    #varlen_attr
                    #[inline(always)]
                    #varlen_vis fn #varlen_uninit_ident(self: ::core::pin::Pin<&mut Self>) -> &mut [::core::mem::MaybeUninit<#varlen_elem_ty>] {
                        let offsets = self.as_ref().header.offsets();
                        unsafe { ::variable_length::macro_support::slice_mut_ref(self, offsets.#varlen_ident, offsets.#varlen_len_ident) }
                    }
                )*
            }
        }

        #tyvis type #tyname = #hidden_mod_name::#tyname;
    }.into()
}

// fn prune_generics(d: &Generics) -> Generics {
//     d.clone()
// }

fn has_varlen(f: &Field) -> bool {
    let varlen_attr: Attribute = parse_quote!{ #[varlen] };
    for attr in &f.attrs {
        if attr == &varlen_attr {
            return true;
        }
    }
    false
}

struct VarLenFields {
    attrs: Vec<TokenStream>,
    vises: Vec<Visibility>,
    idents: Vec<Ident>,
    mut_idents: Vec<Ident>,
    uninit_idents: Vec<Ident>,
    len_idents: Vec<Ident>,
    elem_tys: Vec<Type>,
    len_exprs: Vec<TokenStream>,
    ty_params: Vec<Ident>,
}

fn parse_varlen_fields(varlen_fields: Punctuated<Field, Comma>) -> VarLenFields {
    let varlen_attr: Attribute = parse_quote!{ #[varlen] };
    let mut attrs = Vec::new();
    let mut vises = Vec::new();
    let mut idents = Vec::new();
    let mut len_idents = Vec::new();
    let mut uninit_idents = Vec::new();
    let mut mut_idents = Vec::new();
    let mut elem_tys = Vec::new();
    let mut len_exprs = Vec::new();
    let mut ty_params = Vec::new();
    // let mut varlen_marker_fields = Vec::new();
    // let mut varlen_init_ty_constraints = Vec::new();
    // let mut varlen_init_tys = Vec::new();
    // let mut varlen_init_fields = Vec::new();
    for (i, f) in varlen_fields.into_iter().enumerate() {
        let span = f.span();
        let f_attrs = f.attrs.into_iter().filter(|attr| attr != &varlen_attr);
        attrs.push(quote_spanned!{span=> #(#f_attrs)* });
        vises.push(lift_field_vis(&f.vis));
        mut_idents.push(format_ident!("{}_mut", f.ident.as_ref().unwrap()));
        uninit_idents.push(format_ident!("{}_uninit", f.ident.as_ref().unwrap()));
        len_idents.push(format_ident!("{}_len", f.ident.as_ref().unwrap()));
        idents.push(f.ident.unwrap());
        let (elem_ty, len_expr) = match f.ty {
            Type::Array(a) => {
                let len = a.len;
                (*a.elem, quote_spanned!{len.span()=> #len})
            },
            _ => {
                let span = f.ty.span();
                (f.ty, quote_spanned!{span => 
                    ::core::compile_error!("Fields annotated with #[varlen] must be of array type")
                })
            },
        };
        elem_tys.push(elem_ty);
        len_exprs.push(len_expr);
        ty_params.push(format_ident!("Init{}", i));
    }
    VarLenFields{attrs, vises, idents, mut_idents, uninit_idents, len_idents, elem_tys, len_exprs, ty_params}
}

// fn min_visibility<'t>(a: &'t Visibility, b: impl Iterator<Item= &'t Visibility>) -> &'t Visibility {
//     let mut r = a;
//     for v in b {
//         if lt_visibility(v, r) {
//             r = v;
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

fn parse_header_fields(header_fields: &Punctuated<Field, Comma>) -> (Vec<TokenStream>, Vec<Visibility>, Vec<&Ident>, Vec<&Type>) {
    let mut attrs = Vec::new();
    let mut idents = Vec::new();
    let mut tys = Vec::new();
    let mut vis = Vec::new();
    for f in header_fields {
        let f_attrs = &f.attrs;
        attrs.push(quote_spanned!{f.span() => #( #f_attrs )* });
        idents.push(f.ident.as_ref().unwrap());
        tys.push(&f.ty);
        vis.push(lift_field_vis(&f.vis));
    }
    (attrs, vis, idents, tys)
}

fn lift_field_vis(a: &Visibility) -> Visibility {
    match a {
        Visibility::Public(_) => a.clone(),
        Visibility::Crate(_) => a.clone(),
        Visibility::Restricted(r) => {
            if r.path.is_ident("crate") {
                a.clone()
            } else if r.path.is_ident("super") {
                parse_quote!{ pub(in super::super) }
            } else if r.path.is_ident("self") {
                parse_quote!{ pub(super) }
            } else {
                panic!("Unexpected visibility; pub(in foo) is not supported")
            }
        },
        Visibility::Inherited => {
            parse_quote!{ pub(super) }  
        },
    }
}