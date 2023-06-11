use proc_macro::TokenStream;
use syn::{braced, parse_macro_input, token, Field, Ident, Result, Token};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;

struct Typed {


}

impl Parse for Typed {
	fn parse( input: ParseStream ) -> Result<Self> {

	}
}

#[derive(Debug)]
struct Decl {
	name: Ident,
	brace_token: token::Brace,
	fields: Punctuated<Typed, Token![;]>,
}

impl Parse for Decl {
	fn parse( input: ParseStream ) -> Result<Self> {
		let content;
		Ok( Decl {
			name: input.parse()?,
			brace_token: braced!( content in input ),
			fields: content.parse_terminated( Typed::parse, Token![;] )?,
		})
	}
}

#[proc_macro]
pub fn gen_from_structure( tokens: TokenStream ) -> TokenStream {
	let input = parse_macro_input!( tokens as Decl );
	let mut code = vec![ Token![struct] ];

	TokenStream::from( vec![] )
}