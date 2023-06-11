use proc_macro::TokenStream;

use syn::{braced, Expr, Ident, parse_macro_input, Result, token, Token};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;

struct Typed {
	typ: Ident,
	name: Ident,
	squares: Option<Expr>
}

impl Parse for Typed {
	fn parse( input: ParseStream ) -> Result<Self> {
		Ok( Typed {
			typ: input.parse()?,
			name: input.parse()?,
			squares: input.parse()?,
		})
	}
}

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
	let mut code = String::from("struct ");

	code += &input.name.to_string();
	code += "{\n";
	for field in input.fields {
		code += "pub ";
		code += &field.name.to_string();
		code += ": ";
		code += &field.typ.to_string();
		code += ",\n";
	}
	code += "}";

	code.parse().unwrap()
}