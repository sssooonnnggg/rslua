use rslua::compiler::*;
use rslua::lexer::*;
use rslua::parser::*;

fn try_compile(input: &str) -> Proto {
    let mut lexer = Lexer::new();
    lexer.set_debug(true);
    if let Ok(tokens) = lexer.run(input) {
        let mut parser = Parser::new();
        parser.set_debug(true);
        if let Ok(block) = parser.run(tokens) {
            let mut compiler = Compiler::new();
            let proto = compiler.run(&block);
            println!("{:?}", proto);
            return proto;
        }
    }
    unreachable!()
}

fn try_compile_to_string(input:&str) -> String {
    format!("{:?}", try_compile(input))
}

mod compiler_tests {
    use super::*;
    
    #[test]
    fn empty_block() {
        assert_eq!(
            try_compile_to_string(";"),
            r#"instructions :
| line  | OP         | A     | B     | C     |
| 1     | Return     | 0     | 1     |       |
"#);
    }
}
