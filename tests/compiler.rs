use rslua::compiler::*;
use rslua::lexer::*;
use rslua::parser::*;
use rslua::proto::Proto;

fn try_compile(input: &str) -> Proto {
    let mut lexer = Lexer::new();
    lexer.set_debug(true);
    if let Ok(tokens) = lexer.run(input) {
        let mut parser = Parser::new();
        parser.set_debug(true);
        if let Ok(block) = parser.run(tokens) {
            let mut compiler = Compiler::new();
            if let Ok(proto) = compiler.run(&block) {
                println!("{:?}", proto);
                return proto;
            }
        }
    }
    unreachable!()
}

fn try_compile_and_print(input: &str) -> String {
    format!("{:?}", try_compile(input))
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
stack size : 3
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
}
