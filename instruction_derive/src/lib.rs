extern crate proc_macro;

#[macro_use]
extern crate quote;

use convert_case::{Boundary, Case, Casing};
use proc_macro::TokenStream;
use proc_macro2::Ident;
use syn::{
    parse_macro_input, punctuated::Punctuated, Attribute, DeriveInput, Expr, Lit, MetaNameValue,
    Token, Variant,
};

#[proc_macro_derive(InstructionImpl, attributes(instruction, instruction_struct_name))]
pub fn derive_instruction(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(item as DeriveInput);
    let attrs: Vec<&Attribute> = input
        .attrs
        .iter()
        .filter(|attr| attr.path().is_ident("instruction_struct_name"))
        .collect();

    let attr = attrs.first().expect("missing struct name");

    let instruction_struct_name: Ident = attr.parse_args().unwrap();
    let opcode_struct_name = input.ident.clone();

    let instruction_impl =
        impl_instruction_constructor_funcs(input, &instruction_struct_name, &opcode_struct_name);

    let instruction_struct = quote! {
        #[derive(Debug)]
        pub struct #instruction_struct_name {
            opcode: #opcode_struct_name,
            instruction_type: InstructionType,
            regs: Vec<String>,
            cycles: Vec<usize>,
            data: Vec<u8>,
        }

    };

    let tokens = quote! {
        #instruction_struct
        #instruction_impl
    };

    TokenStream::from(tokens)
}

fn impl_instruction_constructor_funcs(
    input: DeriveInput,
    instruction_struct_name: &Ident,
    opcode_struct_name: &Ident,
) -> proc_macro2::TokenStream {
    let opcode_enum = match &input.data {
        syn::Data::Enum(opcode_enum) => opcode_enum,
        _ => panic!("should be enum"),
    };

    let instruction_constructors: Vec<proc_macro2::TokenStream> = opcode_enum
        .variants
        .iter()
        .filter_map(|variant| impl_instruction_constructor_fun(variant, &opcode_struct_name))
        .collect();

    let instruction_impl = quote! {
        impl #instruction_struct_name {
                pub fn opcode(&self) -> &#opcode_struct_name {
                    &self.opcode
                }

                pub fn instruction_type(&self) -> &InstructionType {
                    &self.instruction_type
                }

                pub fn cycles(&self, branched: bool) -> usize {
                    if branched {
                        *self.cycles.first().unwrap()
                    } else {
                        *self.cycles.last().unwrap()
                    }
                }

                pub fn data(&self) -> &[u8] {
                    &self.data
                }

                pub fn regs(&self) -> &Vec<String> {
                    &self.regs
                }

                #(#instruction_constructors)*
        }
    };

    instruction_impl
}

fn impl_instruction_constructor_fun(
    variant: &Variant,
    opcode_struct_name: &Ident,
) -> Option<proc_macro2::TokenStream> {
    let attrs: Vec<&Attribute> = variant
        .attrs
        .iter()
        .filter(|attr| attr.path().is_ident("instruction"))
        .collect();

    if attrs.is_empty() {
        return None;
    }

    let opcode = variant.ident.to_string();
    let opcode_constructor_name = opcode
        .with_boundaries(&[Boundary::Underscore])
        .to_case(Case::Snake);

    let attr = attrs.first().unwrap();

    let name_values: Punctuated<MetaNameValue, Token![,]> =
        attr.parse_args_with(Punctuated::parse_terminated).unwrap();

    let mut instruction_type_name: Option<String> = None;
    let mut regs = vec![];
    let mut expected_data_len: usize = 0;
    let mut cycles = vec![];

    for nv in name_values {
        let name = nv.path.get_ident().unwrap().to_string();

        match name.as_str() {
            "regs" => {
                match nv.value {
                    Expr::Array(reg_list) => {
                        for reg in reg_list.elems {
                            match reg {
                                Expr::Lit(l) => match l.lit {
                                    Lit::Str(v) => regs.push(quote! {
                                        regs.push(#v.to_string());
                                    }),
                                    _ => panic!("invalid regs type"),
                                },
                                _ => panic!("invalid regs type"),
                            }
                        }
                    }
                    _ => panic!("invalid regs type"),
                };
            }
            "instruction_type" => {
                let lit = match nv.value {
                    Expr::Lit(v) => v.lit,
                    _ => panic!("incorrect type"),
                };

                let value = match lit {
                    Lit::Str(v) => v.value(),
                    _ => panic!("incorrect type"),
                };

                instruction_type_name = Some(value);
            }
            "len" => {
                let lit = match nv.value {
                    Expr::Lit(v) => v.lit,
                    _ => panic!("incorrect type"),
                };

                expected_data_len = match lit {
                    Lit::Int(v) => v.base10_parse().unwrap(),
                    _ => panic!("incorrect type"),
                };
            }
            "cycles" => match nv.value {
                Expr::Array(cycles_list) => {
                    for cycle in cycles_list.elems {
                        match cycle {
                            Expr::Lit(l) => match l.lit {
                                Lit::Int(v) => cycles.push(quote!( {
                                          cycles.push(#v);
                                })),
                                _ => panic!("invalid regs type"),
                            },
                            _ => panic!("invalid regs type"),
                        }
                    }
                }
                _ => panic!("invalid regs type"),
            },
            _ => panic!("unknown argument: {}", name),
        };
    }

    let opcode_constructor_name_ident = format_ident!("{}", opcode_constructor_name);
    let instruction_type_name_ident = format_ident!("{}", instruction_type_name.unwrap());
    let opcode_ident = format_ident!("{}", opcode);

    Some(quote! {
        pub fn #opcode_constructor_name_ident(data: &[u8]) -> Self {
            if data.len() != #expected_data_len {
                panic!(
                    "invalid data provided for {:?}. expected: {}, got: {}",
                        #opcode_struct_name::#opcode_ident,
                        #expected_data_len,
                        data.len()
                    );
            }

            let mut regs = vec![];

            #(#regs)*

            let mut cycles = vec![];

            #(#cycles)*

            Self {
                opcode: #opcode_struct_name::#opcode_ident,
                instruction_type: InstructionType::#instruction_type_name_ident,
                cycles,
                regs,
                data: data.into(),
            }
        }
    })
}
