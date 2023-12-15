use std::{
    io::Write,
    process::{Command, Stdio},
};

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::{quote, quote_spanned};
use syn::{
    parse_macro_input, spanned::Spanned, Attribute, Data, DeriveInput, Expr, ExprLit, Fields,
    GenericArgument, Lit, Path, PathArguments, Type, TypePath,
};

fn unwrap_angles<'a>(ty: &'a Type, match_ident: &str) -> std::option::Option<&'a Type> {
    let segments = match ty {
        Type::Path(TypePath {
            qself: None,
            path: Path {
                segments,

                leading_colon: _,
            },
        }) => segments,
        _ => return None,
    };
    let Some(first) = segments.first() else {
        return None;
    };
    if first.ident != match_ident {
        return None;
    };
    let PathArguments::AngleBracketed(ref firstarg) = first.arguments else {
        return None;
    };
    let Some(GenericArgument::Type(inner_ty)) = firstarg.args.first() else {
        return None;
    };
    Some(inner_ty)
}

enum FindResult {
    FindIdent(Ident),
    FindBadAttribute(Span),
}

fn find_builder_each(attrs: &[Attribute]) -> std::option::Option<FindResult> {
    let builder = attrs.iter().find(|a| a.path().is_ident("builder"))?;
    let args: Expr = builder.parse_args().ok()?;
    let Expr::Assign(ref ass) = args else {
        return None;
    };
    let lhs = match ass.left.as_ref() {
        Expr::Path(lhs) => lhs,
        _ => return None,
    };
    if lhs.path.is_ident("each") {
        let rhs = match ass.right.as_ref() {
            Expr::Lit(ExprLit {
                attrs: _,
                lit: Lit::Str(rhs),
            }) => rhs,
            _ => {
                return None;
            }
        };
        Some(FindResult::FindIdent(rhs.parse().ok()?))
    } else {
        Some(FindResult::FindBadAttribute(args.span()))
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let builder_ident = Ident::new(&format!("{name}Builder"), Span::call_site());

    let fields = match input.data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => fields,
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    };

    let new_builder_body_iter = fields.named.iter().map(|f| {
        let name = &f.ident;
        let attrs = &f.attrs;
        if find_builder_each(attrs).is_some() {
            quote_spanned!(f.span()=> #name: vec![])
        } else {
            quote_spanned!(f.span()=> #name: None)
        }
    });
    let new_builder_body = quote!(#(#new_builder_body_iter ,)*);

    let builder_body_iter = fields.named.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        let attrs = &f.attrs;
        if find_builder_each(attrs).is_some() {
            let inner_ty = unwrap_angles(ty, "Vec").unwrap();
            quote_spanned!(f.span()=> #name: Vec<#inner_ty>)
        } else if let Some(inner_ty) = unwrap_angles(ty, "Option") {
            quote_spanned!(f.span()=> #name: std::option::Option<#inner_ty>)
        } else {
            quote_spanned!(f.span()=> #name: std::option::Option<#ty>)
        }
    });
    let builder_body = quote!(#(#builder_body_iter ,)*);
    let mut setters_iter = vec![];

    for f in fields.named.iter() {
        let name = &f.ident;
        let ty = &f.ty;
        let attrs = &f.attrs;
        if let Some(findresult) = find_builder_each(attrs) {
            let inner_ty = unwrap_angles(ty, "Vec").unwrap();
            match findresult {
                FindResult::FindIdent(each) => {
                    setters_iter.push(
                    quote_spanned!(f.span() => fn #each(&mut self, #each: #inner_ty)-> &mut Self {
                            self.#name.push(#each);
                            self
                        }
                    ),
                );
                }
                FindResult::FindBadAttribute(span) => setters_iter.push(
                    quote_spanned!(span => compile_error!("expected `builder(each = \"...\")`");),
                ),
            }
        } else if let Some(inner_ty) = unwrap_angles(ty, "Option") {
            setters_iter.push(
                quote_spanned!(f.span() => fn #name(&mut self, #name: #inner_ty)-> &mut Self {
                        self.#name = Some(#name);
                        self
                    }
                ),
            );
        } else {
            setters_iter.push(
                quote_spanned!(f.span() => fn #name(&mut self, #name: #ty)-> &mut Self {
                        self.#name = Some(#name);
                        self
                    }
                ),
            );
        }
    }
    let setters = quote!(#(#setters_iter)*);
    let build_body_iter = fields.named.iter().map(|f| {
        let name = &f.ident;
        let name_str = name.as_ref().map(|ii| ii.to_string()).unwrap_or_default();
        let name_err = format!("{} not set", name_str);
        let ty = &f.ty;
        let attrs = &f.attrs;

        if unwrap_angles(ty, "Option").is_some() || find_builder_each(attrs).is_some() {
            quote_spanned!(f.span() =>
            let #name = self.#name.clone();
            )
        } else {
            quote_spanned!(f.span() =>
            let #name = self.#name.clone().ok_or_else(|| #name_err )?;
            )
        }
    });
    let build_body_fields = fields.named.iter().map(|f| {
        let name = &f.ident;

        quote_spanned!(f.span() =>
            #name
        )
    });
    let build_body = quote!(
        #(#build_body_iter)*
        Ok(
            #name {
                #(#build_body_fields,)*
            }
        )
    );

    let expanded = quote! {
        pub struct #builder_ident {
            #builder_body
        }
        impl #name {
            pub fn builder()->#builder_ident{
                #builder_ident{
                    #new_builder_body
                }
            }
        }
        impl #builder_ident {
            #setters
            pub fn build(&mut self)->std::result::Result<#name, std::boxed::Box<dyn std::error::Error>>{
                #build_body
            }
        }
    };
    {
        let expanded_str = &expanded.to_string();
        eprintln!("{expanded_str}");
        let mut command = Command::new("rustfmt")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("command failed");
        {
            let mut cmd_in = command.stdin.take().expect("could not open stdin");
            cmd_in
                .write_all(expanded_str.as_bytes())
                .expect("Failed to write to stdin");
        }
        let check_output = command.wait_with_output().expect("Command failed");

        eprintln!("{}", String::from_utf8_lossy(&check_output.stdout));
    }
    TokenStream::from(expanded)
}
