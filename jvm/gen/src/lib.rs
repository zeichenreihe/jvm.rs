use proc_macro::TokenStream;

use quote::quote;
use syn::{braced, bracketed, Expr, Ident, parse_macro_input, Result, token, Token};
use syn::__private::TokenStream2;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;

struct Typed {
	type_width: Option<usize>,
	typ: TokenStream2,
	name: Ident,
	squares: Option<Expr>
}

impl Parse for Typed {
	fn parse( input: ParseStream ) -> Result<Self> {
		let typ: Ident = input.parse()?;

		let type_width = match typ.to_string().as_str() {
			"u1" => Some(1),
			"u2" => Some(2),
			"u4" => Some(4),
			_ => None,
		};

		let typ: TokenStream2 = match typ.to_string().as_str() {
			"u1" => "u8",
			"u2" => "u16",
			"u4" => "u32",
			i => i,
		}.parse()?;

		let name = input.parse()?;

		let squares = if input.peek( token::Bracket ) {
			let content;
			bracketed!( content in input );
			content.call( Expr::parse ).map_or( None, |x| Some(x) )
		} else {
			None
		};

		Ok( Typed { type_width, typ, name, squares } )
	}
}

struct Declaration {
	name: Ident,
	fields: Punctuated<Typed, Token![;]>,
}

impl Parse for Declaration {
	fn parse( input: ParseStream ) -> Result<Self> {
		let name = input.parse()?;
		let content;
		braced!( content in input );
		let fields = content.parse_terminated( Typed::parse, Token![;] )?;

		Ok( Self { name, fields } )
	}
}

#[proc_macro]
pub fn declare_jvm_struct(tokens: TokenStream ) -> TokenStream {
	let input = parse_macro_input!( tokens as Declaration );

	let name = input.name;

	let mut fields = Vec::with_capacity(input.fields.len());
	let mut load = Vec::with_capacity(input.fields.len());
	let mut load_struct = Vec::with_capacity(input.fields.len());
	for field in input.fields {
		let typ = field.typ;
		let name = field.name;
		let squares = field.squares;

		fields.push( if let Some(_) = field.type_width {
			quote! {
				pub #name: #typ,
			}
		} else {
			quote! {
				pub #name: Vec<#typ>,
			}
		});

		load.push(if let Some(n) = field.type_width {
			quote! {
				let #name = {
					let mut buf = [0u8; #n];
					let length = reader.read(&mut buf)?;
					if length == #n {
						#typ::from_be_bytes(buf)
					} else {
						Err(Error::ReadWrongAmountOfData { actual: length, expected: #n, string: stringify!(#name)})?
					}
				};
			}
		} else {
			quote! {
				let #name = {
					let mut vec = Vec::with_capacity((#squares) as usize);
					for _ in 0..((#squares) as usize) {
						vec.push(#typ::load(reader)?);
					}
					vec
				};
			}
		});
		load_struct.push(
			quote! {
				#name,
			}
		)
	}

	let exp = quote! {
		#[derive(Debug)]
		struct #name {
			#( #fields )*
		}

		impl Load for #name {
			fn load<R: Read>(reader: &mut R) -> Result<#name, Error> {
				#( #load )*
				Ok( Self {
					#( #load_struct )*
				} )
			}
		}
	};

	exp.into()
}
