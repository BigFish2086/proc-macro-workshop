use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;
use syn::visit_mut::VisitMut;

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    assert!(args.is_empty(), "this attribute takes no arguments");

    let mut out = input.clone();
    let ty = parse_macro_input!(input as syn::Item);
    if let Err(e) = sorted_variants(ty) {
        out.extend(TokenStream::from(e.to_compile_error()));
    }
    out
}

fn sorted_variants(input: syn::Item) -> Result<(), syn::Error> {
    if let syn::Item::Enum(e) = input {
        let mut names = Vec::new();
        for variant in e.variants.iter() {
            let name = &variant.ident;
            if names.last().map(|last| &name < last).unwrap_or(false) {
                let next_lext_i = names.binary_search(&name).unwrap_err();
                return Err(syn::Error::new(
                    name.span(),
                    format!("{} should sort before {}", name, names[next_lext_i]),
                ));
            }
            names.push(name);
        }
        Ok(())
    } else {
        Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "expected enum or match expression",
        ))
    }
}

#[derive(Default)]
struct LexoigraphicMatching {
    errors: Vec<syn::Error>,
}

impl syn::visit_mut::VisitMut for LexoigraphicMatching {
    fn visit_expr_match_mut(&mut self, node: &mut syn::ExprMatch) {
        if node.attrs.iter().any(|a| a.path().is_ident("sorted")) {
            // remove the sorted attribute
            node.attrs.retain(|a| !a.path().is_ident("sorted"));
            let mut wild = None;
            let mut names = Vec::new();
            for arm in node.arms.iter() {
                if let Some(ref w) = wild {
                    self.errors.push(syn::Error::new_spanned(&w, "wildcard pattern should come last"));
                    break;
                }
                let path = if let Some(path) = get_arm_path(&arm.pat) {
                    path
                } else if let syn::Pat::Wild(w) = &arm.pat {
                    wild = Some(w);
                    break;
                } else {
                    self.errors.push(syn::Error::new_spanned(&arm.pat, "unsupported by #[sorted]"));
                    break;
                };
                let name = get_path_name(&path);
                if names.last().map(|last| &name < last).unwrap_or(false) {
                    let next_lext_i = names.binary_search(&name).unwrap_err();
                    self.errors.push(syn::Error::new_spanned(
                        path,
                        format!("{} should sort before {}", name, names[next_lext_i]),
                    ));
                }
                names.push(name);
            }
        }
        // keep recursing
        syn::visit_mut::visit_expr_match_mut(self, node);
    }
}

fn get_path_name(path: &syn::Path) -> String {
    path.segments
        .iter()
        .map(|s| s.ident.to_string())
        .collect::<Vec<_>>()
        .join("::")
}

fn get_arm_path(arm: &syn::Pat) -> Option<syn::Path> {
    match *arm {
        syn::Pat::Ident(syn::PatIdent {
            ident: ref id,
            ..
        }) => Some(id.clone().into()),
        syn::Pat::Path(ref p) => Some(p.path.clone()),
        syn::Pat::Struct(ref s) => Some(s.path.clone()),
        syn::Pat::TupleStruct(ref s) => Some(s.path.clone()),
        _ => None,
    }
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    assert!(args.is_empty(), "this attribute takes no arguments");

    let mut f = parse_macro_input!(input as syn::ItemFn);
    let mut lm = LexoigraphicMatching::default();

    lm.visit_item_fn_mut(&mut f);
    let mut ts = quote!(#f);
    ts.extend(lm.errors.into_iter().take(1).map(|e| e.to_compile_error()));
    ts.into()
}
