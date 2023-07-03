use proc_macro::TokenStream as TokenStream1;
use quote::quote;
use syn::{Block, braced, bracketed, Expr, Ident, ItemImpl, parse_macro_input, parse_quote, Result, token, Token};
use proc_macro2::TokenStream;
use syn::fold::{Fold, fold_expr};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;

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
	value_to_read: Option<Expr>,
}

impl Parse for Typed {
	fn parse(input: ParseStream) -> Result<Self> {
		let typ = SpecialType::parse(input)?;
		let name = input.parse()?;
		let squares = if input.peek( token::Bracket ) {
			let content;
			bracketed!( content in input );
			Some(content.call( Expr::parse )?)
		} else {
			None
		};
		let value_to_read = if input.peek(Token![=]) {
			input.parse::<Token![=]>()?;
			Some(input.parse()?)
		} else {
			None
		};
		if squares.is_some() && value_to_read.is_some() {
			Err(input.error("cannot use [] and = at the same time."))
		} else {
			Ok(Typed { typ, name, squares, value_to_read })
		}
	}
}

struct EnumTyped {
	name: Ident,
	fields: Punctuated<Typed, Token![;]>,
}

impl Parse for EnumTyped {
	fn parse(input: ParseStream) -> Result<Self> {
		Ok(EnumTyped {
			name: input.parse()?,
			fields: {
				let content;
				braced!(content in input);
				content.parse_terminated( Typed::parse, Token![;])?
			},
		})
	}
}

struct ParsingImplFolder<'a>(&'a Punctuated<EnumTyped, Token![,]>);

impl<'a> Fold for ParsingImplFolder<'a> {
	fn fold_expr(&mut self, i: Expr) -> Expr {
		match i {
			Expr::Path(path) if path.path.segments.len() == 2 && path.path.segments[0].ident.to_string() == "Self" => {
				let f = self.0.iter()
					.filter(|v| v.name == path.path.segments[1].ident)
					.next();
				if let Some(variant) = f {
					let name = &variant.name;

					let mut fields = Vec::with_capacity(variant.fields.len());
					let mut fields_load = Vec::with_capacity(variant.fields.len());

					for field in &variant.fields {
						let name = &field.name;
						let typ = &field.typ.0;

						fields_load.push(if let Some(squares) = &field.squares { quote! {
							let #name = {
								let mut vec = Vec::with_capacity((#squares) as usize);
								for _ in 0..((#squares) as usize) {
									vec.push(#typ::parse(reader, constant_pool)?);
								}
								vec
							};
						}} else { quote!{
							let #name = {
								#typ::parse(reader, constant_pool)?
							};
						}
						});

						fields.push(quote!{
							#name,
						});
					}

					return parse_quote!(
						{
							#( #fields_load )*
							Self::#name { #( #fields )* }
						}
					);
				} else {
					fold_expr(self, Expr::Path(path))
				}
			},
			i => fold_expr(self, i),
		}
	}
}

enum Declaration {
	Struct {
		name: Ident,
		fields: Punctuated<Typed, Token![;]>,
		braces: Option<Block>,
	},
	Enum {
		name: Ident,
		variants: Punctuated<EnumTyped, Token![,]>,
		parser: Option<ItemImpl>,
	}
}

impl Parse for Declaration {
	fn parse(input: ParseStream) -> Result<Self> {
		if input.peek(Token![enum]) {
			input.parse::<Token![enum]>()?;
			let name = input.parse()?;
			let variants = {
				let content;
				braced!(content in input);
				content.parse_terminated(EnumTyped::parse, Token![,])?
			};
			let parser = if input.peek(Token![impl]) {
				let mut folder = ParsingImplFolder(&variants);
				Some(folder.fold_item_impl(input.parse()?))
			} else {
				None
			};
			Ok(Self::Enum {
				name,
				variants,
				parser,
			})
		} else if input.peek(Token![struct]) {
			input.parse::<Token![struct]>()?;
			Ok(Self::Struct {
				name: input.parse()?,
				fields: {
					let content;
					braced!( content in input );
					content.parse_terminated( Typed::parse, Token![;])?
				},
				braces: if input.peek(token::Brace) {
					Some(input.parse()?)
				} else {
					None
				},
			})
		} else {
			Err(input.error("expected `struct` or `enum`."))?
		}
	}
}

#[proc_macro]
pub fn declare_jvm_struct(tokens: TokenStream1) -> TokenStream1 {
	let input = parse_macro_input!( tokens as Declaration );

	match input {
		Declaration::Enum { name, variants, parser } => {
			let mut variants_out = Vec::with_capacity(variants.len());
			for variant in variants {
				let name = variant.name;
				let mut fields = Vec::with_capacity(variant.fields.len());
				for field in variant.fields {
					let name = field.name;
					let typ = field.typ.0;
					fields.push(if field.squares.is_some() { quote!{
						#name: Vec<#typ>,
					}} else { quote!{
						#name: #typ,
					}});
				}

				variants_out.push(quote!{
					#name { #( #fields )* },
				});
			}

			let exp = quote!{
				#[derive(Debug, Clone, PartialEq, Eq)]
				pub enum #name {
					#( #variants_out )*
				}

				#parser
			};
			exp.into()
		},
		Declaration::Struct { name: struct_name, fields, braces, } => {
			let braces = braces.map_or_else(|| Vec::new(), |b| b.stmts);

			let mut fields_out = Vec::with_capacity(fields.len());
			let mut parse_out = Vec::with_capacity(fields.len());
			let mut parse_out_struct = Vec::with_capacity(fields.len());

			let mut has_seen_constant_pool_field = false;
			for field in fields {
				let constant_pool = if has_seen_constant_pool_field && struct_name == "ClassFile" { quote!{
					Some(&constant_pool)
				}} else { quote!{
					constant_pool
				}};

				let name = field.name;
				let typ = field.typ.0;

				has_seen_constant_pool_field |= name == "constant_pool";

				fields_out.push(if field.squares.is_some() { quote!{
					pub #name: Vec<#typ>,
				}} else { quote!{
					pub #name: #typ,
				}});

				parse_out.push(if let Some(squares) = field.squares { quote!{
					let #name = {
						let mut vec = Vec::with_capacity((#squares) as usize);
						for _ in 0..((#squares) as usize) {
							vec.push(#typ::parse(reader, #constant_pool)?);
						}
						vec
					};
				}} else if let Some(value_to_read) = field.value_to_read { quote!{
					let #name = {
						#typ::parse(reader, #constant_pool)?
					};
					if #name != #value_to_read {
						return Err(Error::ReadDifferentValueThanExpected(format!(stringify!(#struct_name #name expected #value_to_read, got {}), #name)));
					}
				}} else { quote! {
					let #name = {
						#typ::parse(reader, #constant_pool)?
					};
				}});

				parse_out_struct.push(quote!{
					#name,
				})
			}

			let exp = quote! {
				#[derive(Debug, Clone, PartialEq, Eq)]
				pub struct #struct_name {
					#( #fields_out )*
				}

				impl<R: Read> Parse<R> for #struct_name {
					fn parse(reader: &mut R, constant_pool: Option<&Vec<CpInfo>>) -> Result<Self, Error> {
						#( #braces )*
						#( #parse_out )*
						Ok( Self { #( #parse_out_struct )* } )
					}
				}
			};
			exp.into()
		}
	}
}
