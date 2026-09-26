//! `chipi-syntax`: the lexer, recursive-descent parser, AST, source and span model, plus the
//! diagnostics that point at source spans, all for the chipi instruction-set DSL.
//!
//! No dependencies, no `unsafe`.

#![forbid(unsafe_code)]

pub mod ast;
pub mod grammar;
pub mod lex;
pub mod report;
pub mod source;

pub use ast::Spec;
pub use grammar::{parse, parse_expr_str};
pub use report::{Diag, Severity};
pub use source::{Source, Span, Spanned};

#[cfg(test)]
mod tests {
    use super::ast::{BitOrder, DispAttr, DispHint, Endian, Expr, Item, ValueKind};
    use super::*;

    fn one(src: &str) -> Item {
        let spec = parse(src).expect("parse ok");
        assert_eq!(spec.items.len(), 1);
        spec.items.into_iter().next().unwrap()
    }

    #[test]
    fn decoder_header_fields() {
        let Item::Decoder(d) = one("decoder Mips { width = 32 bit_order = lsb0 endian = little }")
        else {
            panic!("expected decoder");
        };
        assert_eq!(d.name.text, "Mips");
        assert_eq!(d.width.unwrap().node, 32);
        assert_eq!(d.bit_order.unwrap().node, BitOrder::Lsb0);
        assert_eq!(d.endian.unwrap().node, Endian::Little);
    }

    #[test]
    fn selector_and_instruction() {
        let src = r#"
            selector op [31:26]
            add op=0 funct=0b100000 rd:greg[15:11] rs:greg[25:21] | "add {rd}, {rs}"
        "#;
        let spec = parse(src).unwrap();
        assert_eq!(spec.items.len(), 2);
        let Item::Instr(i) = &spec.items[1] else {
            panic!("expected instr");
        };
        assert_eq!(i.name.text, "add");
        assert_eq!(i.constraints.len(), 2);
        assert_eq!(i.bindings.len(), 2);
        assert_eq!(i.display.len(), 1);
    }

    #[test]
    fn number_width_hints() {
        let toks = lex::lex("0xAD 0b100000 42").unwrap();
        assert_eq!(
            toks[0].kind,
            lex::TokenKind::Int {
                value: 0xAD,
                width_hint: Some(8)
            }
        );
        assert_eq!(
            toks[1].kind,
            lex::TokenKind::Int {
                value: 0b100000,
                width_hint: Some(6)
            }
        );
        assert_eq!(
            toks[2].kind,
            lex::TokenKind::Int {
                value: 42,
                width_hint: None
            }
        );
    }

    #[test]
    fn value_decl_with_xforms_and_hint() {
        let Item::Value(v) = one("type simm16 = i32 { sign_extend(16), display(signed_hex) }")
        else {
            panic!("expected value decl");
        };
        assert_eq!(v.kind, ValueKind::Type);
        assert_eq!(v.name.text, "simm16");
        assert_eq!(v.base.text, "i32");
        assert_eq!(v.xforms.len(), 1);
        assert!(matches!(v.disp, Some(DispAttr::Hint(DispHint::SignedHex))));
    }

    #[test]
    fn conditional_display_arm() {
        let src = r#"addi op=14 rd:gpr[6:10] | rd == 0 : "li {rd}" | "addi {rd}""#;
        let Item::Instr(i) = one(src) else {
            panic!("expected instr");
        };
        assert_eq!(i.display.len(), 2);
        assert!(i.display[0].cond.is_some());
        assert!(i.display[1].cond.is_none());
    }

    #[test]
    fn unterminated_string_is_an_error() {
        let err = parse("add op=0 | \"oops").unwrap_err();
        assert_eq!(err.code, "LexUnterminatedString");
    }

    #[test]
    fn assemble_expression_parses() {
        let src = r#"jal op=0b1101111 rd:gpr[7:11]
            joff:i21 = assemble 21 { [20] = word[31] [10:1] = word[30:21] [0] = 0 } sign_extend
            | "jal {rd}, {joff}""#;
        let Item::Instr(i) = one(src) else {
            panic!("expected instr")
        };
        assert_eq!(i.computed.len(), 1);
        let Expr::Assemble {
            out_width, parts, ..
        } = &i.computed[0].expr
        else {
            panic!("expected assemble");
        };
        assert_eq!(*out_width, 21);
        assert_eq!(parts.len(), 3);
    }
}
