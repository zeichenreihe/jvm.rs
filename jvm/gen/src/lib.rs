use proc_macro::TokenStream;

use quote::quote;
use syn::{braced, bracketed, Expr, Ident, parse_macro_input, Result, token, Token};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;

struct Typed {
	typ: Ident,
	name: Ident,
	squares: Option<Expr>
}

impl Parse for Typed {
	fn parse( input: ParseStream ) -> Result<Self> {
		let mut typ: Ident = input.parse()?;
		typ = Ident::new(
			match typ.to_string().as_str() {
				"u1" => "u8",
				"u2" => "u16",
				"u4" => "u32",
				i => i,
			},
			typ.span()
		);
		let name = input.parse()?;

		let squares = if input.peek( token::Bracket ) {
			let content;
			bracketed!( content in input );
			content.call( Expr::parse ).map_or( None, |x| Some(x) )
		} else {
			None
		};

		Ok( Typed { typ, name, squares } )
	}
}

struct Decl {
	name: Ident,
	fields: Punctuated<Typed, Token![;]>,
}

impl Parse for Decl {
	fn parse( input: ParseStream ) -> Result<Self> {
		let name = input.parse()?;
		let content;
		braced!( content in input );
		let fields = content.parse_terminated( Typed::parse, Token![;] )?;

		Ok( Decl { name, fields } )
	}
}

#[proc_macro]
pub fn declare_jvm_struct(tokens: TokenStream ) -> TokenStream {
	let input = parse_macro_input!( tokens as Decl );

	let name = input.name;

	let mut fields = Vec::new();
	for field in input.fields {
		let typ = field.typ;
		let name = field.name;
		let squares = field.squares;
		fields.push( quote! { pub #name: #typ, } );
	}

	let exp = quote! {
		#[repr(C, packed)]
		#[derive(Debug, AsBytes, FromBytes)]
		struct #name {
			#( #fields )*
		}
	};

	exp.into()
}
