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
        // eprintln!("ts: {:#?}", ts);

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
    fn expand_token_tree(
        &self,
        stream: TokenTree2,
        rest: &mut proc_macro2::token_stream::IntoIter,
        i: u64,
    ) -> TokenTree2 {
        match stream {
            TokenTree2::Group(ref group) => {
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
            TokenTree2::Ident(mut ident) => {
                let mut peek = rest.clone();
                match (peek.next(), peek.next()) {
                    // example: `func~N`
                    (Some(TokenTree2::Punct(ref punct)), Some(TokenTree2::Ident(ref ident2)))
                        if punct.as_char() == '~' && ident2 == &self.ident => {
                            ident = proc_macro2::Ident::new(&format!("{}{}", ident, i), ident.span()) .into();
                            *rest = peek.clone();
                            match peek.next() {
                                // to consume another '~' if found, example: `func~N~`
                                Some(TokenTree2::Punct(ref punct)) if punct.as_char() == '~' => {
                                    *rest = peek;
                                }
                                _ => {}
                            }
                        }
                    _ => {}
                };
                TokenTree2::Ident(ident)
            }
            tt => tt,
        }
    }

    fn expand_token_stream(&self, stream: TokenStream2, i: u64) -> TokenStream2 {
        let mut out = TokenStream2::new();
        let mut tts = stream.into_iter();
        while let Some(tt) = tts.next() {
            out.extend(std::iter::once(self.expand_token_tree(tt, &mut tts, i)));
        }
        out.into()
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as SeqMacroInput);
    let output: TokenStream2 = input.into();
    output.into()
}
