use proc_macro::{self, TokenStream};
use quote::{__private::ext::RepToTokensExt, format_ident, quote, ToTokens};
use syn::{
    parse_macro_input, punctuated::Punctuated, token::Comma, AngleBracketedGenericArguments, Data,
    DataEnum, DataStruct, DeriveInput, Fields, GenericArgument, Item, ItemMod, PathArguments, Type,
    TypePath, Variant,
};

/// Derive for generating spannable impls for structs
#[proc_macro_derive(Spannable)]
pub fn span(input: TokenStream) -> TokenStream {
    let DeriveInput { ident, data, .. } = parse_macro_input!(input);
    let tokens = match data {
        Data::Struct(_) => {
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
        Data::Enum(DataEnum { variants, .. }) => {
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
        Data::Union(_) => unimplemented!(),
    };
    tokens.into()
}

/// Derive for generating spannable impls for structs
#[proc_macro_derive(Typeable)]
pub fn ty(input: TokenStream) -> TokenStream {
    let DeriveInput { ident, data, .. } = parse_macro_input!(input);
    let tokens = match data {
        Data::Struct(_) => {
            quote! {
                impl jswt_common::Typeable for #ident {
                    fn ty(&self) -> jswt_common::Type {
                        self.ty.to_owned()
                    }
                    fn binding(&self) -> Option<std::borrow::Cow<'static, str>> {
                        match &self.ty {
                            jswt_common::Type::Binding(r) => Some(r.clone()),
                            _ => None
                        }
                    }
                }
            }
        }
        // This is currently a low effort macro for generating  Spannable implementation for enums
        // It assumes that every variant of the enum also implements Spannable
        Data::Enum(DataEnum { variants, .. }) => {
            let variant_ident = variants.into_iter().map(|v| v.ident);
            let variant_ident_ = variant_ident.clone();
            quote! {
                impl jswt_common::Typeable for #ident {
                    fn ty(&self) -> jswt_common::Type {
                        match self {
                            #(
                                #ident::#variant_ident(variant) => variant.ty(),
                            )*
                        }
                    }

                    fn binding(&self) -> Option<std::borrow::Cow<'static, str>> {
                        match self {
                            #(
                                #ident::#variant_ident_(variant) => variant.binding(),
                            )*
                        }
                    }
                }
            }
        }
        Data::Union(_) => unimplemented!(),
    };
    tokens.into()
}

/// Genarates From<EnumVariant> for EnumDeclaration
#[proc_macro_derive(FromEnumVariant)]
pub fn enum_variant(input: TokenStream) -> TokenStream {
    let DeriveInput { ident, data, .. } = parse_macro_input!(input);
    let tokens = match data {
        Data::Enum(DataEnum { variants, .. }) => variants
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
        // Unsupported variants for now
        Data::Struct(_) => unimplemented!(),
        Data::Union(_) => unimplemented!(),
    };

    let tokens = quote! { #(#tokens)* };
    tokens.into()
}


#[proc_macro]
pub fn derive_visitors(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree representation
    let input = parse_macro_input!(input as ItemMod);

    // // Extract the module name
    let mod_name = input.ident;
    let content = input.content.unwrap();

    // Generate visitor traits and visitable implementations for each struct
    let mut visitor_impls = vec![];
    let mut visitor_fns = vec![];
    for item in content.1 {
        match item {
            Item::Enum(ref enum_item) => {
                let visitor_impl = quote! {
                    #[derive(Visitor)]
                    #enum_item
                };

                visitor_impls.push(visitor_impl);

                let enum_name = &enum_item.ident;
                let visit_fn_name =
                    format_ident!("visit_{}", to_snake_case(&enum_name.to_string()));
                let walker_fn_name =
                    format_ident!("walk_{}", to_snake_case(&enum_name.to_string()));
                let visit_fn = quote! {
                    fn #visit_fn_name(&mut self, node: &#enum_name){
                        #walker_fn_name(self, node);
                    }
                };
                visitor_fns.push(visit_fn);
            }
            Item::Struct(ref struct_item) => {
                let visitor_impl = quote! {
                    #[derive(Visitor)]
                    #struct_item
                };
                visitor_impls.push(visitor_impl);

                let struct_name = &struct_item.ident;
                let visit_fn_name =
                    format_ident!("visit_{}", to_snake_case(&struct_name.to_string()));
                let walker_fn_name =
                    format_ident!("walk_{}", to_snake_case(&struct_name.to_string()));
                let visit_fn = quote! {
                    fn #visit_fn_name(&mut self, node: &#struct_name){
                        #walker_fn_name(self, node);
                    }
                };
                visitor_fns.push(visit_fn);
            }
            _ => {}
        }
    }

    // Combine the generated code into a single output token stream
    let output = quote! {
        #[allow(non_snake_case)]
        pub mod #mod_name {
            use super::*;
            #(#visitor_impls)*

            pub trait Visitor: Sized {
                #(#visitor_fns)*
            }
        }
    };
    output.into()
}

/// Derive the `Visitor` trait for a struct.
/// The input token stream is expected to contain a struct definition, and the output token stream
/// will contain the implementation of the visitor trait for the struct.
///
/// The function will panic if the input token stream does not contain a struct definition.
#[proc_macro_derive(Visitor, attributes(walk))]
pub fn derive_visitor(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let mut visit_statements = vec![];
    match &input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => create_visit_statements_from_struct_fields(fields, &mut visit_statements),
        Data::Enum(DataEnum { variants, .. }) => {
            create_visit_statements_from_enum(variants, name, &mut visit_statements)
        }
        _ => panic!("Visitor derive only supports named fields"),
    };

    let walk_fn_name = format_ident!("walk_{}", to_snake_case(&name.to_string()));
    let output = quote! {
        pub fn #walk_fn_name<V: Visitor>(visitor: &mut V, node: &#name) {
            #(#visit_statements)*
        }
    };
    output.into()
}

fn create_visit_statements_from_enum(
    variants: &Punctuated<Variant, Comma>,
    name: &proc_macro2::Ident,
    visit_statements: &mut Vec<proc_macro2::TokenStream>,
) {
    let mut visitor_impls = vec![];
    for variant in variants {
        let variant_name = &variant.ident;
        match &variant.fields {
            Fields::Unnamed(fields) => {
                // assert that the variant has only one field
                // TODO handle multiple fields
                assert_eq!(fields.unnamed.len(), 1, "Variant has more than one field");
                for field in fields.unnamed.iter() {
                    let field_type_name = &field.ty.to_token_stream().to_string();
                    let field_type_name = format_ident!("visit_{}", to_snake_case(field_type_name));
                    let q = quote! {
                        #name::#variant_name(variant) => visitor.#field_type_name(variant),
                    };
                    visitor_impls.push(q);
                }
            }
            _ => panic!("Visitor derive only supports unnamed fields"),
        };
    }

    let out = quote! {
        match node {
            #(#visitor_impls)*
        }
    };
    visit_statements.push(out);
}

fn create_visit_statements_from_struct_fields(
    fields: &syn::FieldsNamed,
    visit_statements: &mut Vec<proc_macro2::TokenStream>,
) {
    let fields = fields.named.clone();
    let walkable_fields = fields
        .iter()
        .filter(|field| field.attrs.iter().any(|attr| attr.path().is_ident("walk")));

    for field in walkable_fields {
        let field_name = field.ident.as_ref().unwrap();
        let mut field_ty = field.ty.clone();
        let field_is_option = type_is_option(&field_ty);
        if field_is_option {
            field_ty = get_option_inner_type(&field_ty);
        }

        let visit_statement = if type_is_vec(&field_ty) {
            // Get the type of the generic parameter of the vec
            let generic_param = match field_ty {
                Type::Path(TypePath { path, .. }) => {
                    let segment = path.segments.last().unwrap();
                    let generic = segment.arguments.clone();
                    generic
                }
                _ => panic!("Expected a type path"),
            };

            match generic_param {
                PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) => {
                    let generic_param = args.first().unwrap().clone();
                    match generic_param {
                        GenericArgument::Type(ty) => {
                            field_ty = ty.clone();
                            let type_name = to_snake_case(&field_ty.to_token_stream().to_string());
                            let visit_fn_name = format_ident!("visit_{}", type_name);

                            if field_is_option {
                                quote! {
                                    if let Some(items) = &node.#field_name {
                                        for item in items {
                                            visitor.#visit_fn_name(&item);
                                        }
                                    }
                                }
                            } else {
                                quote! {
                                    for item in &node.#field_name {
                                        visitor.#visit_fn_name(&item);
                                    }
                                }
                            }
                        }
                        _ => panic!("Expected a type"),
                    }
                }
                _ => panic!("Expected angle bracketed generic arguments"),
            }
        } else if type_is_box(&field_ty) {
            // Get the type of the generic parameter of the box
            let generic_param = match field_ty {
                Type::Path(TypePath { path, .. }) => {
                    let segment = path.segments.last().unwrap();
                    let generic = segment.arguments.clone();
                    generic
                }
                _ => panic!("Expected a type path"),
            };

            match generic_param {
                PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) => {
                    let generic_param = args.first().unwrap().clone();
                    match generic_param {
                        GenericArgument::Type(ty) => {
                            field_ty = ty.clone();
                            let type_name = to_snake_case(&field_ty.to_token_stream().to_string());
                            let visit_fn_name = format_ident!("visit_{}", type_name);

                            if field_is_option {
                                quote! {
                                    if let Some(node) = &node.#field_name {
                                        visitor.#visit_fn_name(&*node);
                                    }
                                }
                            } else {
                                quote! {
                                    visitor.#visit_fn_name(&*node.#field_name);
                                }
                            }
                        }
                        _ => panic!("Expected a type"),
                    }
                }
                _ => panic!("Expected angle bracketed generic arguments"),
            }
        } else if type_is_option(&field_ty) {
            // Get the type of the generic parameter of the option
            let generic_param = match field_ty {
                Type::Path(TypePath { path, .. }) => {
                    let segment = path.segments.last().unwrap();
                    let generic = segment.arguments.clone();
                    generic
                }
                _ => panic!("Expected a type path"),
            };

            match generic_param {
                PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) => {
                    let generic_param = args.first().unwrap().clone();
                    match generic_param {
                        GenericArgument::Type(ty) => {
                            field_ty = ty.clone();
                            let type_name = to_snake_case(&field_ty.to_token_stream().to_string());
                            let visit_fn_name = format_ident!("visit_{}", type_name);

                            if field_is_option {
                                quote! {
                                    if let Some(item) = &node.#field_name {
                                        visitor.#visit_fn_name(&item);
                                    }
                                }
                            } else {
                                quote! {
                                    visitor.#visit_fn_name(&node.#field_name);
                                }
                            }
                        }
                        _ => panic!("Expected a type"),
                    }
                }
                _ => panic!("Expected angle bracketed generic arguments"),
            }
        } else {
            let type_name = to_snake_case(&field_ty.to_token_stream().to_string());
            let visit_fn_name = format_ident!("visit_{}", type_name);

            if field_is_option {
                quote! {
                    if let Some(node) = &node.#field_name {
                        visitor.#visit_fn_name(&node);
                    }
                }
            } else {
                quote! {
                    visitor.#visit_fn_name(&node.#field_name);
                }
            }
        };

        visit_statements.push(visit_statement);
    }
}

fn get_option_inner_type(field_ty: &Type) -> Type {
    // Get the type of the generic parameter of the option
    let generic_param = match field_ty {
        Type::Path(TypePath { path, .. }) => {
            let segment = path.segments.last().unwrap();
            let generic = segment.arguments.clone();
            generic
        }
        _ => panic!("Expected a type path"),
    };

    match generic_param {
        PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) => {
            let generic_param = args.first().unwrap().clone();
            match generic_param {
                GenericArgument::Type(ty) => ty.clone(),
                _ => panic!("Expected a type"),
            }
        }
        _ => panic!("Expected angle bracketed generic arguments"),
    }
}

/// Convert a Pascal/Camel case string to a snake case string
/// e.g. CamelCase -> camel_case
fn to_snake_case(value: &str) -> String {
    let mut snake_case = String::new();
    let mut prev_char_is_upper = false;

    for (i, c) in value.char_indices() {
        if c.is_uppercase() {
            if i > 0 && !prev_char_is_upper {
                snake_case.push('_');
            }
            snake_case.push(c.to_ascii_lowercase());
            prev_char_is_upper = true;
        } else {
            snake_case.push(c);
            prev_char_is_upper = false;
        }
    }

    snake_case
}

fn type_is_vec(field_ty: &Type) -> bool {
    match &field_ty {
        Type::Path(TypePath { path, .. }) => {
            let segment = path.segments.last().unwrap();
            segment.ident == "Vec"
        }
        _ => false,
    }
}

fn type_is_box(field_ty: &Type) -> bool {
    match &field_ty {
        Type::Path(TypePath { path, .. }) => {
            let segment = path.segments.last().unwrap();
            segment.ident == "Box"
        }
        _ => false,
    }
}

fn type_is_option(field_ty: &Type) -> bool {
    match &field_ty {
        Type::Path(TypePath { path, .. }) => {
            let segment = path.segments.last().unwrap();
            segment.ident == "Option"
        }
        _ => false,
    }
}
