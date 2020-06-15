# rslua 

[![Crates.io](https://img.shields.io/crates/v/rslua)](https://crates.io/crates/rslua)

Yet another Lua lexer and Lua parser for Lua 5.3 written in pure Rust.

## Lexer

- **input** str
- **output** Result<Vec\<Token>, LexError>

```rust
use rslua::lexer::Lexer;
let mut lexer = Lexer::new();
let tokens = lexer.run(input_lua_code)?;
```

### Lexer Config

| Key | Type | Default | Descripten | 
| --- | --- | --- | --- |
| `use_origin_string` | bool | false | Use origin string as token value instead of escaped one. |
| `reserve_comments` | bool | false | Reserve comments as tokens. |

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

Read Lua source files from `./lua` folder, parse them, generate ASTs and walk them through, use a `LuaWritter` struct which impletements the `AstVisitor` trait to re-generate formatted Lua source again to `./tmp` folder.

See [tests/lua_to_lua.rs](tests/lua_to_lua.rs)
