use proc_macro::TokenStream as TokenStream1;
use quote::quote;
use syn::{braced, bracketed, Expr, ExprLit, Ident, parenthesized, parse_macro_input, Result, token, Token};
use proc_macro2::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::{Enum, Struct};

#[derive(Debug)]
struct SpecialType(TokenStream);

impl Parse for SpecialType {
	fn parse(input: ParseStream) -> Result<Self> {
		let typ: Ident = input.parse()?;
		Ok(SpecialType(match typ.to_string().as_str() {
				"u1" => "u8",
				"u2" => "u16",
				"u4" => "u32",
				i => i,
			}.parse()?
		))
	}
}

struct Typed {
	typ: SpecialType,
	name: Ident,
	squares: Option<Expr>,
}

impl Parse for Typed {
	fn parse(input: ParseStream) -> Result<Self> {
		Ok(Typed {
			typ: SpecialType::parse(input)?,
			name: input.parse()?,
			squares: if input.peek( token::Bracket ) {
				let content;
				bracketed!( content in input );
				Some(content.call( Expr::parse )?)
			} else {
				None
			},
		})
	}
}

struct EnumTyped {
	index: ExprLit,
	name: Ident,
	fields: Punctuated<Typed, Token![;]>,
}

impl Parse for EnumTyped {
	fn parse(input: ParseStream) -> Result<Self> {
		let index = input.parse()?;
		input.parse::<Token![=]>()?;
		Ok(EnumTyped {
			index,
			name: input.parse()?,
			fields: {
				let content;
				braced!(content in input);
				content.parse_terminated( Typed::parse, Token![;])?
			},
		})
	}
}

enum Declaration {
	Struct {
		name: Ident,
		fields: Punctuated<Typed, Token![;]>,
		braces: Option<Expr>,
	},
	Enum {
		name: Ident,
		tag_type: SpecialType,
		variants: Punctuated<EnumTyped, Token![,]>,
	}
}

impl Parse for Declaration {
	fn parse(input: ParseStream) -> Result<Self> {
		let lookahead = input.lookahead1();
		if lookahead.peek(Enum) {
			input.parse::<Token![enum]>()?;
			Ok(Self::Enum {
				name: input.parse()?,
				tag_type: {
					let content;
					parenthesized!(content in input);
					SpecialType::parse(&content)?
				},
				variants: {
					let content;
					braced!(content in input);
					content.parse_terminated(EnumTyped::parse, Token![,])?
				},
			})
		} else if lookahead.peek(Struct) {
			input.parse::<Token![struct]>()?;
			Ok(Self::Struct {
				name: input.parse()?,
				fields: {
					let content;
					braced!( content in input );
					content.parse_terminated( Typed::parse, Token![;])?
				},
				braces: if input.peek(token::Brace) {
					let content;
					braced!(content in input);
					Some(content.call(Expr::parse)?)
				} else {
					None
				},
			})
		} else {
			Err(lookahead.error())?
		}
	}
}

#[proc_macro]
pub fn declare_jvm_struct(tokens: TokenStream1) -> TokenStream1 {
	let input = parse_macro_input!( tokens as Declaration );
	match input {
		Declaration::Struct { name, fields, braces} => {
			let mut field_declarations = Vec::with_capacity(fields.len());
			let mut load = Vec::with_capacity(fields.len());
			let mut load_struct = Vec::with_capacity(fields.len());
			for field in fields {
				let typ = field.typ.0;
				let name = field.name;

				field_declarations.push( if field.squares.is_some() { quote! {
					pub #name: Vec<#typ>,
				}} else { quote!{
					pub #name: #typ,
				}});

				load.push(if let Some(squares) = field.squares { quote!{
					let #name = {
						let mut vec = Vec::with_capacity((#squares) as usize);
						for _ in 0..((#squares) as usize) {
							vec.push(#typ::load(reader)?);
						}
						vec
					};
				}} else { quote!{
					let #name = #typ::load(reader)?;
				}});

				load_struct.push(quote!{
					#name,
				})
			}

			let implementation = if let Some(braces) = braces { quote! {
				#braces
			} } else { quote!{
				#( #load )*
				Ok(Self {
					#( #load_struct )*
				})
			}};

			let exp = quote!{
				#[derive(Debug)]
				pub struct #name {
					#( #field_declarations )*
				}

				impl Load for #name {
					fn load<R: Read>(reader: &mut R) -> Result<#name, Error> {
						#implementation
					}
				}
			};

			exp.into()
		},
		Declaration::Enum {
			name,
			tag_type: SpecialType(tag_type),
			variants
		} => {
			let mut variants_out = Vec::with_capacity(variants.len());
			let mut variants_match = Vec::with_capacity(variants.len());

			for variant in variants {
				let name = variant.name;
				let index = variant.index;

				let mut field_declarations = Vec::with_capacity(variant.fields.len());
				let mut field_load = Vec::with_capacity(variant.fields.len());
				let mut field_load_struct = Vec::with_capacity(variant.fields.len());

				for field in variant.fields {
					let typ = field.typ.0;
					let name = field.name;

					field_declarations.push(if field.squares.is_some() { quote! {
						#name: Vec<#typ>,
					}} else { quote! {
						#name: #typ,
					}});

					field_load.push(if let Some(squares) = field.squares { quote!{
						let #name = {
							let mut vec = Vec::with_capacity((#squares) as usize);
							for _ in 0..((#squares) as usize) {
								vec.push(#typ::load(reader)?);
							}
							vec
						};
					}} else { quote! {
						let #name = #typ::load(reader)?;
					}});

					field_load_struct.push(quote!{
						#name,
					});
				}

				variants_out.push( quote!{
					#name { #( #field_declarations )* },
				});

				variants_match.push(quote!{
					#index => {
						#( #field_load )*
						Ok( Self::#name {
							#( #field_load_struct )*
						})
					},
				});
			}

			let exp = quote!{
				#[derive(Debug)]
				pub enum #name {
					#( #variants_out )*
				}
				impl Load for #name {
					fn load<R: Read>(reader: &mut R) -> Result<#name, Error> {
						let tag = #tag_type::load(reader)?;

						match tag {
							#( #variants_match )*
							_ => Err(Error::ReadWrongTag { actual: tag as u32, string: stringify!(tag: #tag_type) })
						}
					}
				}
			};

			exp.into()
		},
	}
}
