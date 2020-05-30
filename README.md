# Rua

Yet another Lua lexer and Lua parser for Lua 5.3.

## Lexer

- **input** str
- **output** Result<Vec\<Token>, LexError>

```rust
use rua::lexer::Lexer;
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

Use `ast_walker` to travel the AST, implement the `ast_visitor` trait to run custom logic.

## A complete example

Read `lua/json.lua` , parse it, walk the AST and generate formatted lua code to `json_output.lua`.

See [tests/lua_to_lua.rs](tests/lua_to_lua.rs)