use proc_macro::TokenStream;

use proc_macro2::TokenStream as TokenStream2;
use proc_macro2::TokenTree as TokenTree2;

use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, Result, Token};

#[derive(Debug)]
struct SeqMacroInput {
    ident: syn::Ident,
    from: syn::LitInt,
    to: syn::LitInt,
    ts: TokenStream2,
}

impl Parse for SeqMacroInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = syn::Ident::parse(input)?;
        let _in = <Token![in]>::parse(input)?;
        let from = syn::LitInt::parse(input)?;
        let _dots = <Token![..]>::parse(input)?; // TODO: support `..=`
        let to = syn::LitInt::parse(input)?;

        let content;
        let _braced = syn::braced!(content in input);
        let ts = TokenStream2::parse(&content)?;
        eprintln!("ts: {:#?}", ts);

        Ok(SeqMacroInput {
            ident,
            from,
            to,
            ts,
        })
    }
}

impl Into<TokenStream2> for SeqMacroInput {
    fn into(self) -> TokenStream2 {
        let from = self.from.base10_parse::<u64>().unwrap();
        let to = self.to.base10_parse::<u64>().unwrap();
        (from..to)
            .map(|i| self.expand_token_stream(self.ts.clone(), i))
            .collect()
    }
}

impl SeqMacroInput {
    fn expand_token_tree(&self, stream: TokenTree2, i: u64) -> TokenTree2 {
        match stream {
            TokenTree2::Group(group) => {
                let mut expanded = proc_macro2::Group::new(
                    group.delimiter(),
                    self.expand_token_stream(group.stream(), i),
                );
                expanded.set_span(group.span());
                TokenTree2::Group(expanded)
            }
            TokenTree2::Ident(ref ident) if ident == &self.ident => {
                // TokenTree2::Literal(syn::parse2(quote_spanned! {ident.span()=> #i}).unwrap())
                let mut lit = proc_macro2::Literal::u64_unsuffixed(i);
                lit.set_span(ident.span());
                TokenTree2::Literal(lit)
            }
            tt => tt,
        }
    }

    fn expand_token_stream(&self, stream: TokenStream2, i: u64) -> TokenStream2 {
        stream
            .into_iter()
            .map(|tt| self.expand_token_tree(tt, i))
            .collect()
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as SeqMacroInput);
    let output: TokenStream2 = input.into();
    output.into()
}
