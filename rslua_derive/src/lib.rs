use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Traceable)]
pub fn traceable_macro(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let ast = parse_macro_input!(input as DeriveInput);
    let struct_name = ast.ident;

    quote! {
        impl #struct_name {
            #[cfg(debug_assertions)]
            fn trace_error<T, E: Error>(error: E) -> Result<T, E> {
                if cfg!(debug_assertions) {
                    panic!("{}", error.what());
                } else {
                    eprintln!("{}", error.what());
                    Err(error)
                }
            }
        };
    }
    .into()
}
