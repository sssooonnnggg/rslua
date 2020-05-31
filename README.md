# rslua

Yet another Lua lexer and Lua parser for Lua 5.3.

## Lexer

- **input** str
- **output** Result<Vec\<Token>, LexError>

```rust
use rslua::lexer::Lexer;
let mut lexer = Lexer::new();
let tokens = lexer.run(input_lua_code)?;
```

## Parser

- **input** Vec\<Token>
- **output** Result<Block, ParseError>

```rust
let mut parser = Parser::new();
let block = parser.run(tokens)?;
```

## AST walker

Use `ast_walker` to travel the AST, implement the `AstVisitor` trait to run custom logic.

## A complete example

Read Lua source files, parse them, generate ASTs and walk them through, use a `LuaWritter` struct which impletements the `AstVisitor` trait to re-generate formatted Lua source again.

See [tests/lua_to_lua.rs](tests/lua_to_lua.rs)