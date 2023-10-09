use proc_macro::TokenStream;

use proc_macro2::TokenStream as TokenStream2;
use proc_macro2::TokenTree as TokenTree2;

use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, Result, Token};

#[derive(Debug)]
struct SeqMacroInput {
    ident: syn::Ident,
    from: syn::LitInt,
    inclusive: bool,
    to: syn::LitInt,
    ts: TokenStream2,
}

impl Parse for SeqMacroInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = syn::Ident::parse(input)?;
        let _in = <Token![in]>::parse(input)?;
        let from = syn::LitInt::parse(input)?;
        let inclusive = input.peek(Token![..=]);
        if inclusive {
            <Token![..=]>::parse(input)?;
        } else {
            <Token![..]>::parse(input)?;
        }
        let to = syn::LitInt::parse(input)?;

        let content;
        let _braced = syn::braced!(content in input);
        let ts = TokenStream2::parse(&content)?;

        Ok(SeqMacroInput {
            ident,
            from,
            inclusive,
            to,
            ts,
        })
    }
}

impl Into<TokenStream2> for SeqMacroInput {
    fn into(self) -> TokenStream2 {
        self.expand_token_stream(self.ts.clone())
    }
}

#[derive(Debug, Clone, Copy)]
enum Mode {
    ReplaceIdent(u64),
    ReplaceSequence,
}

impl SeqMacroInput {
    fn range(&self) -> impl Iterator<Item = u64> {
        let value = |lit: &syn::LitInt| lit.base10_parse::<u64>().unwrap();
        let from = value(&self.from);
        let to = value(&self.to);
        if self.inclusive {
            from..(to + 1)
        } else {
            from..to
        }
    }

    fn expand_token_tree(
        &self,
        stream: TokenTree2,
        rest: &mut proc_macro2::token_stream::IntoIter,
        mutated: &mut bool,
        mode: Mode,
    ) -> TokenStream2 {
        let tt = match stream {
            TokenTree2::Group(ref group) => {
                let (expanded, group_mutated) = self.expand_pass(group.stream(), mode);
                let mut expanded = proc_macro2::Group::new(group.delimiter(), expanded);
                *mutated |= group_mutated;
                expanded.set_span(group.span());
                TokenTree2::Group(expanded)
            }
            TokenTree2::Ident(ref ident) if ident == &self.ident => {
                if let Mode::ReplaceIdent(i) = mode {
                    let mut lit = proc_macro2::Literal::u64_unsuffixed(i);
                    lit.set_span(ident.span());
                    TokenTree2::Literal(lit)
                } else {
                    ident.clone().into()
                }
            }
            TokenTree2::Ident(mut ident) => {
                let mut peek = rest.clone();
                match (mode, peek.next(), peek.next()) {
                    // example: `func~N`
                    (
                        Mode::ReplaceIdent(i),
                        Some(TokenTree2::Punct(ref punct)),
                        Some(TokenTree2::Ident(ref ident2)),
                    ) if punct.as_char() == '~' && ident2 == &self.ident => {
                        ident = proc_macro2::Ident::new(&format!("{}{}", ident, i), ident.span())
                            .into();
                        *rest = peek.clone();
                        *mutated = true;
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
            TokenTree2::Punct(ref punct) if punct.as_char() == '#' => {
                // expand sequence, example: ~(...)*
                if let Mode::ReplaceSequence = mode {
                    let mut peek = rest.clone();
                    match (peek.next(), peek.next()) {
                        (Some(TokenTree2::Group(ref rep)), Some(TokenTree2::Punct(ref punct)))
                            if rep.delimiter() == proc_macro2::Delimiter::Parenthesis
                                && punct.as_char() == '*' =>
                        {
                            *mutated = true;
                            *rest = peek;
                            return self
                                .range()
                                .map(|i: u64| self.expand_pass(rep.stream(), Mode::ReplaceIdent(i)))
                                .map(|(ts, _)| ts)
                                .collect();
                        }
                        _ => {}
                    }
                }
                TokenTree2::Punct(punct.clone())
            }
            tt => tt,
        };
        std::iter::once(tt).collect()
    }

    fn expand_pass(&self, stream: TokenStream2, mode: Mode) -> (TokenStream2, bool) {
        let mut out = TokenStream2::new();
        let mut tts = stream.into_iter();
        let mut mutated = false;
        while let Some(tt) = tts.next() {
            out.extend(self.expand_token_tree(tt, &mut tts, &mut mutated, mode));
        }
        (out.into(), mutated)
    }

    fn expand_token_stream(&self, stream: TokenStream2) -> TokenStream2 {
        let (out, mutated) = self.expand_pass(stream.clone(), Mode::ReplaceSequence);
        if mutated {
            return out;
        }
        self.range()
            .map(|i: u64| self.expand_pass(stream.clone(), Mode::ReplaceIdent(i)))
            .map(|(ts, _)| ts)
            .collect()
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as SeqMacroInput);
    let output: TokenStream2 = input.into();
    output.into()
}
