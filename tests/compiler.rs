use rslua::compiler::*;
use rslua::lexer::*;
use rslua::parser::*;
use rslua::proto::Proto;

fn try_compile(input: &str) -> Result<Proto, CompileError> {
    let mut lexer = Lexer::new();
    lexer.set_debug(true);
    if let Ok(tokens) = lexer.run(input) {
        let mut parser = Parser::new();
        parser.set_debug(true);
        if let Ok(block) = parser.run(tokens) {
            let mut compiler = Compiler::new();
            match compiler.run(&block) {
                Ok(proto) => {
                    println!("{:?}", proto);
                    return Ok(proto);
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }
    }
    unreachable!()
}

fn try_compile_and_print(input: &str) -> String {
    match try_compile(input) {
        Ok(proto) => format!("{:?}", proto),
        Err(e) => format!("{}", e.0),
    }
}

mod compiler_tests {
    use super::*;

    #[test]
    fn empty_block() {
        assert_eq!(
            try_compile_and_print(";"),
            r#"
stack size : 2
consts :
locals :
instructions :
| line  | OP         | A     | B     | C     |
| 1     | Return     | 0     | 1     |       |
"#
        );
    }

    #[test]
    fn local_stat() {
        assert_eq!(
            try_compile_and_print("local a, b, c"),
            r#"
stack size : 3
consts :
locals :
| 0     | a          |
| 1     | b          |
| 2     | c          |
instructions :
| line  | OP         | A     | B     | C     |
| 1     | LoadNil    | 0     | 2     |       |
| 2     | Return     | 0     | 1     |       |
"#
        );
    }

    #[test]
    fn local_stat_with_const() {
        assert_eq!(
            try_compile_and_print("local a, b, c = 1, 2.0, '123'"),
            r#"
stack size : 3
consts :
| 0     | 1          |
| 1     | 2          |
| 2     | "123"      |
locals :
| 0     | a          |
| 1     | b          |
| 2     | c          |
instructions :
| line  | OP         | A     | B     | C     |
| 1     | LoadK      | 0     | 0     |       |
| 2     | LoadK      | 1     | 1     |       |
| 3     | LoadK      | 2     | 2     |       |
| 4     | Return     | 0     | 1     |       |
"#
        );
    }

    #[test]
    fn local_stat_with_duplicate_consts() {
        let stat1 = "local a, b, c, d, e, f = 1, 2.0, '123', 1, 2.00, [[123]]";
        let stat2 = "local a, b, c = 1, 2.0, '123'; local d, e, f = 1, 2.00, [[123]]";
        let output = r#"
stack size : 6
consts :
| 0     | 1          |
| 1     | 2          |
| 2     | "123"      |
locals :
| 0     | a          |
| 1     | b          |
| 2     | c          |
| 3     | d          |
| 4     | e          |
| 5     | f          |
instructions :
| line  | OP         | A     | B     | C     |
| 1     | LoadK      | 0     | 0     |       |
| 2     | LoadK      | 1     | 1     |       |
| 3     | LoadK      | 2     | 2     |       |
| 4     | LoadK      | 3     | 0     |       |
| 5     | LoadK      | 4     | 1     |       |
| 6     | LoadK      | 5     | 2     |       |
| 7     | Return     | 0     | 1     |       |
"#;
        assert_eq!(try_compile_and_print(stat1), output);
        assert_eq!(try_compile_and_print(stat2), output);
    }

    #[test]
    fn local_stat_nil_bool() {
        assert_eq!(
            try_compile_and_print("local a, b, c = true, nil, false"),
            r#"
stack size : 3
consts :
locals :
| 0     | a          |
| 1     | b          |
| 2     | c          |
instructions :
| line  | OP         | A     | B     | C     |
| 1     | LoadBool   | 0     | 1     | 0     |
| 2     | LoadNil    | 1     | 0     |       |
| 3     | LoadBool   | 2     | 0     | 0     |
| 4     | Return     | 0     | 1     |       |
"#
        )
    }

    #[test]
    fn local_stat_move() {
        assert_eq!(
            try_compile_and_print("local a, b = 1, 2; local c, d, e, f, g = a, b, 3;"),
            r#"
stack size : 7
consts :
| 0     | 1          |
| 1     | 2          |
| 2     | 3          |
locals :
| 0     | a          |
| 1     | b          |
| 2     | c          |
| 3     | d          |
| 4     | e          |
| 5     | f          |
| 6     | g          |
instructions :
| line  | OP         | A     | B     | C     |
| 1     | LoadK      | 0     | 0     |       |
| 2     | LoadK      | 1     | 1     |       |
| 3     | Move       | 2     | 0     |       |
| 4     | Move       | 3     | 1     |       |
| 5     | LoadK      | 4     | 2     |       |
| 6     | LoadNil    | 5     | 1     |       |
| 7     | Return     | 0     | 1     |       |
"#
        )
    }

    #[test]
    fn assign_simple() {
        assert_eq!(
            try_compile_and_print(
                "local a, b, c, d, e, f, g = 1, 2, 3; d, e, f, g = a, b;a, b, c = 4, 5, 6;"
            ),
            r#"
stack size : 11
consts :
| 0     | 1          |
| 1     | 2          |
| 2     | 3          |
| 3     | 4          |
| 4     | 5          |
| 5     | 6          |
locals :
| 0     | a          |
| 1     | b          |
| 2     | c          |
| 3     | d          |
| 4     | e          |
| 5     | f          |
| 6     | g          |
instructions :
| line  | OP         | A     | B     | C     |
| 1     | LoadK      | 0     | 0     |       |
| 2     | LoadK      | 1     | 1     |       |
| 3     | LoadK      | 2     | 2     |       |
| 4     | LoadNil    | 3     | 3     |       |
| 5     | Move       | 7     | 0     |       |
| 6     | Move       | 8     | 1     |       |
| 7     | LoadNil    | 9     | 1     |       |
| 8     | Move       | 6     | 10    |       |
| 9     | Move       | 5     | 9     |       |
| 10    | Move       | 4     | 8     |       |
| 11    | Move       | 3     | 7     |       |
| 12    | LoadK      | 7     | 3     |       |
| 13    | LoadK      | 8     | 4     |       |
| 14    | LoadK      | 2     | 5     |       |
| 15    | Move       | 1     | 8     |       |
| 16    | Move       | 0     | 7     |       |
| 17    | Return     | 0     | 1     |       |
"#
        )
    }

    #[test]
    fn assign_swap() {
        assert_eq!(
            try_compile_and_print("local a, b = 1, 2; a, b = b, a"),
            r#"
stack size : 4
consts :
| 0     | 1          |
| 1     | 2          |
locals :
| 0     | a          |
| 1     | b          |
instructions :
| line  | OP         | A     | B     | C     |
| 1     | LoadK      | 0     | 0     |       |
| 2     | LoadK      | 1     | 1     |       |
| 3     | Move       | 2     | 1     |       |
| 4     | Move       | 1     | 0     |       |
| 5     | Move       | 0     | 2     |       |
| 6     | Return     | 0     | 1     |       |
"#
        )
    }

    #[test]
    fn assign_free_extra_reg() {
        assert_eq!(
            try_compile_and_print("local a, b, c; a, b, c = 1, 2, 3, 4, 5;local d, e, f"),
            r#"
stack size : 8
consts :
| 0     | 1          |
| 1     | 2          |
| 2     | 3          |
| 3     | 4          |
| 4     | 5          |
locals :
| 0     | a          |
| 1     | b          |
| 2     | c          |
| 3     | d          |
| 4     | e          |
| 5     | f          |
instructions :
| line  | OP         | A     | B     | C     |
| 1     | LoadNil    | 0     | 2     |       |
| 2     | LoadK      | 3     | 0     |       |
| 3     | LoadK      | 4     | 1     |       |
| 4     | LoadK      | 5     | 2     |       |
| 5     | LoadK      | 6     | 3     |       |
| 6     | LoadK      | 7     | 4     |       |
| 7     | Move       | 2     | 5     |       |
| 8     | Move       | 1     | 4     |       |
| 9     | Move       | 0     | 3     |       |
| 10    | LoadNil    | 3     | 2     |       |
| 11    | Return     | 0     | 1     |       |
"#
        )
    }

    #[test]
    fn const_folding() {
        assert_eq!(
            try_compile_and_print(
                r#"
local a, b = 1 + 2 - 3 * 4 / 6 % 7 ^ 8 & 9 | 10 ~ 11 << 1 >> 2,
(1.2 + 3.4) * 5.6 / 7.8 ^ 9.0
"#
            ),
            r#"
stack size : 2
consts :
| 0     | 15         |
| 1     | 0.00000024104295037190596 |
locals :
| 0     | a          |
| 1     | b          |
instructions :
| line  | OP         | A     | B     | C     |
| 1     | LoadK      | 0     | 0     |       |
| 2     | LoadK      | 1     | 1     |       |
| 3     | Return     | 0     | 1     |       |
"#
        )
    }

    #[test]
    fn const_folding_un_op() {
        let output = try_compile_and_print("local a, b = -1 * 2.0, ~(~(234 * 456))");
        let expected = r#"
stack size : 2
consts :
| 0     | -2         |
| 1     | 106704     |
locals :
| 0     | a          |
| 1     | b          |
instructions :
| line  | OP         | A     | B     | C     |
| 1     | LoadK      | 0     | 0     |       |
| 2     | LoadK      | 1     | 1     |       |
| 3     | Return     | 0     | 1     |       |
"#;
        assert_eq!(output, expected);
    }

    #[test]
    fn divide_by_zero() {
        let result = try_compile_and_print(
            r#"
--
-- test devide by zero
--
local a = 1 // 0"#,
        );
        assert_eq!(result, r#"[compile error] divide by zero at line [5]."#)
    }

    #[test]
    fn code_bin_op() {
        let output = try_compile_and_print("local a, b, c; local d = 1 + a - b * c; local e = d + 1");
        let expected = r#"
stack size : 5
consts :
| 0     | 1          |
locals :
| 0     | a          |
| 1     | b          |
| 2     | c          |
| 3     | d          |
| 4     | e          |
instructions :
| line  | OP         | A     | B     | C     |
| 1     | LoadNil    | 0     | 2     |       |
| 2     | Add        | 3     | 256   | 0     |
| 3     | Mul        | 4     | 1     | 2     |
| 4     | Sub        | 3     | 3     | 4     |
| 5     | Add        | 4     | 3     | 256   |
| 6     | Return     | 0     | 1     |       |
"#;
        assert_eq!(output, expected);
    }

    #[test]
    fn code_bin_op_2() {
        let output = try_compile_and_print("local a, b, c; local d, e, f = a * 3 - b, a / b / c, b + a + c");
        let expected = r#"
stack size : 6
consts :
| 0     | 3          |
locals :
| 0     | a          |
| 1     | b          |
| 2     | c          |
| 3     | d          |
| 4     | e          |
| 5     | f          |
instructions :
| line  | OP         | A     | B     | C     |
| 1     | LoadNil    | 0     | 2     |       |
| 2     | Mul        | 3     | 0     | 256   |
| 3     | Sub        | 3     | 3     | 1     |
| 4     | Div        | 4     | 0     | 1     |
| 5     | Div        | 4     | 4     | 2     |
| 6     | Add        | 5     | 1     | 0     |
| 7     | Add        | 5     | 5     | 2     |
| 8     | Return     | 0     | 1     |       |
"#;
        assert_eq!(output, expected);
    }

    #[test]
    fn code_bin_op_3() {
        let output = try_compile_and_print("local a, b, c, d, e, f; d, e, f = a * 3 - b, a / b / c, b + a + c");
        let expected = r#"
stack size : 9
consts :
| 0     | 3          |
locals :
| 0     | a          |
| 1     | b          |
| 2     | c          |
| 3     | d          |
| 4     | e          |
| 5     | f          |
instructions :
| line  | OP         | A     | B     | C     |
| 1     | LoadNil    | 0     | 5     |       |
| 2     | Mul        | 6     | 0     | 256   |
| 3     | Sub        | 6     | 6     | 1     |
| 4     | Div        | 7     | 0     | 1     |
| 5     | Div        | 7     | 7     | 2     |
| 6     | Add        | 8     | 1     | 0     |
| 7     | Add        | 5     | 8     | 2     |
| 8     | Move       | 4     | 7     |       |
| 9     | Move       | 3     | 6     |       |
| 10    | Return     | 0     | 1     |       |
"#;
        assert_eq!(output, expected);
    }

    #[test]
    fn bin_op_4() {
        let output = try_compile_and_print("local a, b, c; local d = (a - b) * (b - c)");
        let expected = r#"
stack size : 5
consts :
locals :
| 0     | a          |
| 1     | b          |
| 2     | c          |
| 3     | d          |
instructions :
| line  | OP         | A     | B     | C     |
| 1     | LoadNil    | 0     | 2     |       |
| 2     | Sub        | 3     | 0     | 1     |
| 3     | Sub        | 4     | 1     | 2     |
| 4     | Mul        | 3     | 3     | 4     |
| 5     | Return     | 0     | 1     |       |
"#;
        assert_eq!(output, expected);
    }

    #[test]
    fn un_op() {
        let output = try_compile_and_print("local a, b, c; local d = -((-a + ~b + (-c)) * 4)");
        let expected = r#"
stack size : 5
consts :
| 0     | 4          |
locals :
| 0     | a          |
| 1     | b          |
| 2     | c          |
| 3     | d          |
instructions :
| line  | OP         | A     | B     | C     |
| 1     | LoadNil    | 0     | 2     |       |
| 2     | Unm        | 3     | 0     |       |
| 3     | BNot       | 4     | 1     |       |
| 4     | Add        | 3     | 3     | 4     |
| 5     | Unm        | 4     | 2     |       |
| 6     | Add        | 3     | 3     | 4     |
| 7     | Mul        | 3     | 3     | 256   |
| 8     | Unm        | 3     | 3     |       |
| 9     | Return     | 0     | 1     |       |
"#;
        assert_eq!(output, expected);
    }

    #[test]
    fn code_not() {
        let output = try_compile_and_print(
            "local a, b, c, d, e, f = not nil, not false, not true, not 0, not 0.1");
        let expected = r#"
stack size : 6
consts :
locals :
| 0     | a          |
| 1     | b          |
| 2     | c          |
| 3     | d          |
| 4     | e          |
| 5     | f          |
instructions :
| line  | OP         | A     | B     | C     |
| 1     | LoadBool   | 0     | 1     | 0     |
| 2     | LoadBool   | 1     | 1     | 0     |
| 3     | LoadBool   | 2     | 0     | 0     |
| 4     | LoadBool   | 3     | 0     | 0     |
| 5     | LoadBool   | 4     | 0     | 0     |
| 6     | LoadNil    | 5     | 0     |       |
| 7     | Return     | 0     | 1     |       |
"#;
            assert_eq!(output, expected);
    }

    #[test]
    fn code_not_2() {
        let output = try_compile_and_print("local a, b = not (1 + 2); b = not a + 1");
        let expected = r#"
stack size : 3
consts :
| 0     | 1          |
locals :
| 0     | a          |
| 1     | b          |
instructions :
| line  | OP         | A     | B     | C     |
| 1     | LoadBool   | 0     | 0     | 0     |
| 2     | LoadNil    | 1     | 0     |       |
| 3     | Not        | 2     | 0     |       |
| 4     | Add        | 1     | 2     | 256   |
| 5     | Return     | 0     | 1     |       |
"#;
        assert_eq!(output, expected);
    }

    #[test]
    fn code_len() {
        let output = try_compile_and_print("local a; local b = #a");
        let expected = r#"
stack size : 2
consts :
locals :
| 0     | a          |
| 1     | b          |
instructions :
| line  | OP         | A     | B     | C     |
| 1     | LoadNil    | 0     | 0     |       |
| 2     | Len        | 1     | 0     |       |
| 3     | Return     | 0     | 1     |       |
"#;
        assert_eq!(
            output,
            expected
        )
    }

    #[test]
    pub fn code_comp() {
        let output = try_compile_and_print("local a, b; local c = a < b;");
        let expected = r#"
stack size : 3
consts :
locals :
| 0     | a          |
| 1     | b          |
| 2     | c          |
instructions :
| line  | OP         | A     | B     | C     |
| 1     | LoadNil    | 0     | 1     |       |
| 2     | Lt         | 1     | 0     | 1     |
| 3     | Jmp        | 0     | 1     |       |
| 4     | LoadBool   | 2     | 0     | 1     |
| 5     | LoadBool   | 2     | 1     | 0     |
| 6     | Return     | 0     | 1     |       |
"#;
        assert_eq!(output, expected);
    }

    #[test]
    pub fn code_comp_2() {
        let output = try_compile_and_print("local a, b, c, d; local e = a < b <= c > d >= 1 == 2 ~= 3");
    }
}
