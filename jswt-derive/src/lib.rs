use proc_macro::{self, TokenStream};
use quote::{__private::ext::RepToTokensExt, quote};
use syn::{parse_macro_input, DataEnum, DeriveInput};

/// Derive for generating spannable impls for structs
#[proc_macro_derive(Typeable)]
pub fn ty(input: TokenStream) -> TokenStream {
    let DeriveInput { ident, data, .. } = parse_macro_input!(input);
    let tokens = match data {
        syn::Data::Struct(_) => {
            quote! {
                impl jswt_common::Typeable for #ident {
                    fn defined_type(&self) -> Type {
                        self.ty.to_owned()
                    }
                }
            }
        }
        // This is currently a low effort macro for generating  Spannable implementation for enums
        // It assumes that every variant of the enum also implements Spannable
        syn::Data::Enum(DataEnum { variants, .. }) => {
            let variant_ident = variants.into_iter().map(|v| v.ident);
            quote! {
                impl jswt_common::Typeable for #ident {
                    fn defined_type(&self) -> Type {
                        match self {
                            #(
                                #ident::#variant_ident(variant) => variant.defined_type(),
                            )*
                        }
                    }
                }
            }
        }
        syn::Data::Union(_) => unimplemented!(),
    };
    tokens.into()
}

/// Derive for generating spannable impls for structs
#[proc_macro_derive(Spannable)]
pub fn span(input: TokenStream) -> TokenStream {
    let DeriveInput { ident, data, .. } = parse_macro_input!(input);
    let tokens = match data {
        syn::Data::Struct(_) => {
            quote! {
                impl jswt_common::Spannable for #ident {
                    fn span(&self) -> Span {
                        self.span.to_owned()
                    }
                }
            }
        }
        // This is currently a low effort macro for generating  Spannable implementation for enums
        // It assumes that every variant of the enum also implements Spannable
        syn::Data::Enum(DataEnum { variants, .. }) => {
            let variant_ident = variants.into_iter().map(|v| v.ident);
            quote! {
                impl jswt_common::Spannable for #ident {
                    fn span(&self) -> Span {
                        match self {
                            #(
                                #ident::#variant_ident(variant) => variant.span(),
                            )*
                        }
                    }
                }
            }
        }
        syn::Data::Union(_) => unimplemented!(),
    };
    tokens.into()
}

/// Genarates From<EnumVariant> for EnumDeclaration
#[proc_macro_derive(FromEnumVariant)]
pub fn enum_variant(input: TokenStream) -> TokenStream {
    let DeriveInput { ident, data, .. } = parse_macro_input!(input);
    let tokens = match data {
        syn::Data::Struct(_) => unimplemented!(),
        syn::Data::Enum(DataEnum { variants, .. }) => variants
            .into_iter()
            .filter(|v| v.fields.len() == 1)
            .map(|v| {
                let field = v.fields.next().unwrap().iter();
                let var = &v.ident;
                quote! {
                    #(
                        impl From<#field> for #ident {
                            fn from(v: #field) -> Self {
                                Self::#var(v)
                            }
                        }
                    )*
                }
            }),
        syn::Data::Union(_) => unimplemented!(),
    };

    let tokens = quote! { #(#tokens)* };
    tokens.into()
}
