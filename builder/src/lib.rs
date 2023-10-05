use proc_macro::TokenStream;
// use proc_macro2::{Ident, Span};
use quote::{quote, ToTokens};
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
                #name: Vec::new()
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
        let name = &f.ident;
        let ty = &f.ty;
        let set_method = if let Some(inner_ty) = ty_inner_type("Option", &ty) {
            quote! {
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = ::std::option::Option::Some(#name);
                    self
                }
            }
        } else if builder_of(&f).is_some() {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = #name;
                    self
                }
            }
        } else {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = ::std::option::Option::Some(#name);
                    self
                }
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

    let gen = quote! {
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

fn builder_of(f: &syn::Field) -> Option<proc_macro2::Group> {
    for attr in &f.attrs {
        if attr.path().segments.len() == 1 && attr.path().segments[0].ident == "builder" {
            let tts = attr.meta.to_token_stream().into_iter();
            for tt in tts {
                if let proc_macro2::TokenTree::Group(g) = tt {
                    return Some(g);
                }
            }
        }
    }
    None
}

fn extend_method(f: &syn::Field) -> Option<(bool, proc_macro2::TokenStream)> {
    // eprintln!("{:#?}", &f.attrs);
    let name = f.ident.as_ref().unwrap();
    let g = builder_of(f)?;
    let mut iter = g.stream().into_iter();
    match iter.next().unwrap() {
        proc_macro2::TokenTree::Ident(ref i) => assert_eq!(i, "each"),
        x => panic!("expected `each` found {}", x),
    };
    match iter.next().unwrap() {
        proc_macro2::TokenTree::Punct(ref p) => assert_eq!(p.as_char(), '='),
        x => panic!("expected `=` found {}", x),
    };
    let arg = match iter.next().unwrap() {
        proc_macro2::TokenTree::Literal(l) => l,
        x => panic!("expected `string` found {}", x),
    };
    match syn::Lit::new(arg) {
        syn::Lit::Str(ref s) => {
            let arg = syn::Ident::new(&s.value(), s.span());
            let inner_ty = ty_inner_type("Vec", &f.ty).unwrap();
            let method = quote! {
                pub fn #arg(&mut self, #arg: #inner_ty) -> &mut Self {
                    self.#name.push(#arg);
                    self
                }
            };
            return Some((&arg == name, method));
        }
        x => panic!("expected string found {:#?}", x),
    };
}
