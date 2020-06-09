#[allow(unused_must_use)]
mod lexer_tests {
    use rslua::lexer::{LexError, Lexer};
    use rslua::tokens::*;
    use rslua::types::Source;
    use std::fs::File;
    use std::io::prelude::*;

    fn try_lexer(input: &str) -> Vec<Token> {
        let mut lexer = Lexer::new();
        lexer.set_debug(true);
        let tokens = lexer.run(input);
        println!("{:#?}", tokens);
        tokens.ok().unwrap()
    }

    #[test]
    fn lexer_practical() -> std::io::Result<()> {
        let mut file = File::open(r"lua/json.lua")?;
        let mut content = String::new();
        file.read_to_string(&mut content)?;
        try_lexer(&content);
        Ok(())
    }
    #[test]
    fn simple_stat() -> Result<(), LexError> {
        try_lexer("local a = 1;a = a + 1\n b = 2");
        Ok(())
    }
    #[test]
    fn short_comment() -> Result<(), LexError> {
        try_lexer(
            r#"-- this is a short comment
        -- this is anothe short comment
        "#,
        );
        Ok(())
    }
    #[test]
    fn long_comment() -> Result<(), LexError> {
        try_lexer(
            r#"--[[ this is a long comment
        line2
        line3
        line4
        ]]
        "#,
        );
        try_lexer(
            r#"--[==[ this is a long comment
        [line2]
        [[line3]]
        [=[[line4]]]
        ]==]
        "#,
        );
        Ok(())
    }
    #[test]
    #[should_panic]
    fn long_comment_panic1() {
        try_lexer(
            r#"--[[ this is a long comment
        line2
        line3
        line4
        ]
        "#,
        );
    }
    #[test]
    #[should_panic]
    fn long_comment_panic2() {
        try_lexer(
            r#"--[[ this is a long comment
        line2
        line3
        line4
        
        "#,
        );
    }
    #[test]
    #[should_panic]
    fn long_comment_panic3() {
        try_lexer(
            r#"--[==[ this is a long comment
        line2
        line3
        line4]]
        
        "#,
        );
    }
    #[test]
    fn short_string() -> Result<(), LexError> {
        try_lexer(r#""\r\n\u{5B57} \r\n\229\173\151 \r\n\xE5\xAD\x97""#);
        try_lexer(r#""字 字""#);
        try_lexer(r#"'123456'"#);
        try_lexer("'\\z\n    skip spaces\\z\n              line 2'");
        Ok(())
    }
    #[test]
    #[should_panic]
    fn short_string_panic1() {
        try_lexer(r#""\r\n\u{5B57FFFF}""#);
    }
    #[test]
    #[should_panic]
    fn short_string_panic2() {
        try_lexer(r#""\256\256""#);
    }
    #[test]
    fn long_string() -> Result<(), LexError> {
        try_lexer(
            r#"[[ this is a long string
        line2
        line3
        line4]]
        "#,
        );
        try_lexer(
            r#"[==[
        this is a long string
        [line2]
        [[line3]]
        [[[line4]]]
        abcdefghijklmn]==]
        "#,
        );
        try_lexer("[[1234]]");
        try_lexer("[===[1234]===]");
        Ok(())
    }
    #[test]
    fn str_to_int() {
        assert_eq!(Some(0x12345678), Lexer::str_to_int("0x12345678"));
        assert_eq!(Some(0x6789aBcD), Lexer::str_to_int("0x6789aBcD"));
        assert_eq!(Some(-0x6789aBcD), Lexer::str_to_int("-0x6789aBcD"));
        assert_eq!(Some(-0x6789aBcD), Lexer::str_to_int("  -0x6789aBcD    "));
        assert_eq!(Some(0x7FFFFFFF), Lexer::str_to_int("0x7FFFFFFF"));
        assert_eq!(Some(-0x7FFFFFFF), Lexer::str_to_int("-0x7FFFFFFF"));
        assert_eq!(None, Lexer::str_to_int("0x"));
        assert_eq!(None, Lexer::str_to_int(""));
        assert_eq!(Some(12345), Lexer::str_to_int("12345"));
        assert_eq!(Some(12345), Lexer::str_to_int(" 12345  "));
        assert_eq!(Some(12345), Lexer::str_to_int(" +12345  "));
        assert_eq!(Some(-12345), Lexer::str_to_int(" -12345  "));
        assert_eq!(Some(-12345), Lexer::str_to_int(" -012345  "));
        assert_eq!(None, Lexer::str_to_int("123.456"));
        assert_eq!(None, Lexer::str_to_int("hello world"));
        assert_eq!(None, Lexer::str_to_int("12345s"));
        assert_eq!(None, Lexer::str_to_int("0x12345s"));
    }
    #[test]
    fn str_to_float() {
        assert_eq!(Some(0.123456), Lexer::str_to_float("0.123456"));
        assert_eq!(Some(3.0), Lexer::str_to_float("03.00"));
        assert_eq!(Some(314.16e-2), Lexer::str_to_float("314.16e-2"));
        assert_eq!(Some(0.31416E1), Lexer::str_to_float("0.31416E1"));
        assert_eq!(Some(34E1), Lexer::str_to_float("34E1"));
        assert_eq!(Some(0.345), Lexer::str_to_float(".345"));
        assert_eq!(Some(0.1171875), Lexer::str_to_float("0x0.1E"));
        assert_eq!(Some(162.1875), Lexer::str_to_float("0xA23p-4"));
        assert_eq!(
            Some(3.141592653589793),
            Lexer::str_to_float("0X1.921FB54442D18P+1")
        );
        assert_eq!(Some(13e-2), Lexer::str_to_float("13e-2"));
        assert_eq!(None, Lexer::str_to_float("a34E1"));
        assert_eq!(None, Lexer::str_to_float("3.14.1"));
        assert_eq!(None, Lexer::str_to_float("3..14"));
        assert_eq!(None, Lexer::str_to_float("3..14ss"));
    }

    #[test]
    fn number() {
        let tokens = try_lexer("13e-2");
        assert_eq!(
            tokens,
            vec![
                Token {
                    t: TokenType::Flt,
                    value: TokenValue::Float(0.13),
                    source: Source {
                        pos: 0,
                        length: 5,
                        line: 1,
                        col: 1
                    }
                },
                Token {
                    t: TokenType::Eos,
                    value: TokenValue::None,
                    source: Source {
                        pos: 5,
                        length: 0,
                        line: 1,
                        col: 6
                    }
                }
            ]
        )
    }

    #[test]
    fn name() {
        let tokens = try_lexer("codepoint_to_utf8");
        assert_eq!(
            tokens,
            vec![
                Token {
                    t: TokenType::Name,
                    value: TokenValue::Str("codepoint_to_utf8".to_string()),
                    source: Source {
                        pos: 0,
                        length: 17,
                        line: 1,
                        col: 1
                    }
                },
                Token {
                    t: TokenType::Eos,
                    value: TokenValue::None,
                    source: Source {
                        pos: 17,
                        length: 0,
                        line: 1,
                        col: 18
                    }
                }
            ]
        )
    }

    #[test]
    fn idiv() {
        let tokens = try_lexer("//");
        assert_eq!(
            tokens,
            vec![
                Token {
                    t: TokenType::IDiv,
                    value: TokenValue::None,
                    source: Source {
                        pos: 0,
                        length: 2,
                        line: 1,
                        col: 1,
                    },
                },
                Token {
                    t: TokenType::Eos,
                    value: TokenValue::None,
                    source: Source {
                        pos: 2,
                        length: 0,
                        line: 1,
                        col: 3,
                    },
                },
            ],
        )
    }
}
