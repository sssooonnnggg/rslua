use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Debuggable)]
pub fn debuggable_macro(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let ast = parse_macro_input!(input as DeriveInput);
    let struct_name = ast.ident;

    quote! {
        impl #struct_name {
            pub fn set_debug(&mut self, debug: bool) {
                self.debug = debug;
            }
            pub fn is_debug(&self) -> bool {
                self.debug
            }
        };
    }
    .into()
}
