use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    // eprintln!("{:#?}", ast);

    let name = &ast.ident;
    let builder_name = format!("{}Builder", name);
    // let builder_ident = Ident::new(&builder_name, Span::call_site());
    let builder_ident = syn::Ident::new(&builder_name, name.span());

    let fields = match ast.data {
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
            ..
        }) => named,
        _ => unimplemented!(), // in case of Enum or Unit
    };

    let options_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if ty_inner_type("Option", &ty).is_some() || builder_of(&f).is_some() {
            quote! {
                #name: #ty
            }
        } else {
            quote! {
                #name: ::std::option::Option<#ty>
            }
        }
    });

    let none_fields = fields.iter().map(|f| {
        let name = &f.ident;
        if builder_of(&f).is_some() {
            quote! {
                #name: ::std::vec::Vec::new()
            }
        } else {
            quote! {
                #name: ::std::option::Option::None
            }
        }
    });

    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;
        if ty_inner_type("Option", &f.ty).is_some() || builder_of(&f).is_some() {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
            }
        }
    });

    let methods = fields.iter().map(|f| {
        let name = f.ident.as_ref().unwrap();
        let ty = &f.ty;
        let (arg_type, value) = if let Some(inner_ty) = ty_inner_type("Option", &ty) {
            (inner_ty, quote! { ::std::option::Option::Some(#name) })
        } else if builder_of(&f).is_some() {
            (ty, quote! { #name })
        } else {
            (ty, quote! { ::std::option::Option::Some(#name) })
        };
        let set_method = quote! {
            pub fn #name(&mut self, #name: #arg_type) -> &mut Self {
                self.#name = #value;
                self
            }
        };
        match extend_method(&f) {
            None => set_method,
            Some((true, extend_method)) => extend_method,
            Some((false, extend_method)) => quote! {
                #set_method
                #extend_method
            },
        }
    });

    // let mut inner = proc_macro2::TokenStream::new();
    // inner.extend(vec![
    //     proc_macro2::TokenTree::Ident(proc_macro2::Ident::new("doc", proc_macro2::Span::call_site())),
    //     proc_macro2::TokenTree::Punct(proc_macro2::Punct::new('=', proc_macro2::Spacing::Alone)),
    //     proc_macro2::TokenTree::Literal(proc_macro2::Literal::string(&format!("\
    //         Implements the [builder pattern] for [`{}`].\n\
    //         \n\
    //         [builder pattern]: https://rust-lang.github.io/api-guidelines/type-safety.html#c-builder", name)))
    // ]);

    // let mut ts = proc_macro2::TokenStream::new();
    // ts.extend(vec![
    //     proc_macro2::TokenTree::Punct(proc_macro2::Punct::new('#', proc_macro2::Spacing::Alone)),
    //     proc_macro2::TokenTree::Group(proc_macro2::Group::new(proc_macro2::Delimiter::Bracket, inner)),
    // ]);
    //
    let doc = format!("\
        Implements the [builder pattern] for [`{}`].\n\
        \n\
        [builder pattern]: https://rust-lang.github.io/api-guidelines/type-safety.html#c-builder", name);

    let gen = quote! {
        #[doc = #doc]
        pub struct #builder_ident {
            #(#options_fields),*
        }
        impl #builder_ident {
            #(#methods)*
            pub fn build(&self) -> ::std::result::Result<#name, ::std::boxed::Box<dyn ::std::error::Error>> {
                ::std::result::Result::Ok(#name {
                    #(#build_fields),*
                })
            }
        }
        impl #name {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#none_fields),*
                }
            }
        }
    };
    gen.into()
}

fn ty_inner_type<'a>(wrapper: &str, ty: &'a syn::Type) -> Option<&'a syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() != 1 || p.path.segments[0].ident != wrapper {
            return None;
        }
        if let syn::PathArguments::AngleBracketed(ref inner_ty) = p.path.segments[0].arguments {
            if inner_ty.args.len() != 1 {
                return None;
            }
            let inner_ty = inner_ty.args.first().unwrap();
            if let syn::GenericArgument::Type(ref t) = inner_ty {
                return Some(t);
            }
        }
    }
    None
}

fn builder_of(f: &syn::Field) -> Option<&syn::Attribute> {
    for attr in &f.attrs {
        if attr.path().segments.len() == 1 && attr.path().segments[0].ident == "builder" {
            return Some(attr);
        }
    }
    None
}

fn extend_method(f: &syn::Field) -> Option<(bool, proc_macro2::TokenStream)> {
    let name = f.ident.as_ref().unwrap();
    let attr = builder_of(f)?;

    fn mk_err<T: quote::ToTokens>(t: T) -> Option<(bool, proc_macro2::TokenStream)> {
        Some((
            false,
            syn::Error::new_spanned(t, "expected `builder(each = \"...\")`").to_compile_error(),
        ))
    }

    match &attr.meta {
        syn::Meta::List(list) if list.path.is_ident("builder") => {
            // list here is .. in #[builder(..)]
            let mut nvs = match list.parse_args_with(
                syn::punctuated::Punctuated::<syn::Meta, syn::Token![,]>::parse_terminated,
            ) {
                Ok(nested) => nested,
                Err(_) => return mk_err(list), // return Some((false, e.to_compile_error())),
            };
            if nvs.len() != 1 {
                return mk_err(&nvs);
            }

            // only one element here is (hopefully): each = "foo"
            match nvs.pop().unwrap().into_value() {
                syn::Meta::NameValue(nv) if nv.path.is_ident("each") => {
                    let arg = match &nv.value {
                        syn::Expr::Lit(syn::ExprLit {
                            lit: syn::Lit::Str(ref s),
                            ..
                        }) => s,
                        _ => return mk_err(&nvs),
                    };
                    let arg = syn::Ident::new(&arg.value(), name.span());
                    let inner_ty = ty_inner_type("Vec", &f.ty).unwrap();
                    let extend_method = quote! {
                        pub fn #arg(&mut self, #arg: #inner_ty) -> &mut Self {
                            self.#name.push(#arg);
                            self
                        }
                    };
                    return Some((&arg == name, extend_method));
                }
                _ => {
                    return mk_err(&attr.meta);
                }
            }
        }
        _ => {
            return mk_err(&attr);
        }
    };
}
