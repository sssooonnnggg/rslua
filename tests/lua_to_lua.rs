use rslua::ast::*;
use rslua::ast_walker;
use rslua::ast_walker::AstVisitor;
use rslua::lexer::{Lexer, LexerConfig};
use rslua::parser::Parser;
use rslua_traits::Comments;
use std::fs::File;
use std::fs::{create_dir, read_dir};
use std::io::prelude::*;
use std::str;

#[derive(PartialEq)]
enum FuncCallType {
    WithComments,
    WithOutComments,
}

struct LuaWriter {
    output: String,
    indent: usize,
    depth: usize,
    line_start: bool,
    comment_guard: bool,
    func_call_stack: Vec<FuncCallType>,
}

#[allow(dead_code)]
impl LuaWriter {
    pub fn new() -> Self {
        LuaWriter {
            output: String::new(),
            indent: 2,
            depth: 0,
            line_start: true,
            comment_guard: false,
            func_call_stack: vec![],
        }
    }

    pub fn run(&mut self, block: &Block) -> &str {
        self.output.clear();
        ast_walker::walk_block(block, self).unwrap();
        &self.output
    }

    fn append(&mut self, content: &str) {
        self.indent_at_line_start();
        self.output(content);
    }

    fn indent_at_line_start(&mut self) {
        if self.line_start {
            self.output(&" ".repeat(self.depth * self.indent));
            self.line_start = false;
        }
    }

    fn incline(&mut self) {
        self.output("\n");
        self.line_start = true;
    }

    fn incline_if_not_line_start(&mut self) {
        if !self.line_start {
            self.incline();
        }
    }

    fn output(&mut self, content: &str) {
        self.comment_guard = false;
        self.output.push_str(content);
    }

    fn space(&mut self) {
        self.append(" ");
    }

    fn append_space(&mut self, content: &str) {
        self.append(content);
        self.space();
    }

    fn space_append(&mut self, content: &str) {
        self.space();
        self.append(content);
    }

    fn space_append_space(&mut self, content: &str) {
        self.space();
        self.append(content);
        self.space();
    }

    fn append_inc(&mut self, content: &str) {
        self.append(content);
        self.incline();
    }

    fn end(&mut self) {
        self.leave_scope();
        self.append("end");
    }

    fn enter_scope(&mut self) {
        self.depth += 1;
    }

    fn leave_scope(&mut self) {
        self.depth -= 1;
    }

    fn append_and_incline(&mut self, content: &str) {
        self.append(content);
        self.incline();
    }
}

type WriteResult<T> = Result<T, ()>;
type WriteSuccess = WriteResult<()>;

impl AstVisitor for LuaWriter {
    fn stat_sep(&mut self) {
        self.incline();
    }

    fn begin_if(&mut self, _cond: &Expr) -> WriteResult<bool> {
        self.append_space("if");
        Ok(false)
    }

    fn then(&mut self, _block: &Block) -> WriteResult<bool> {
        self.space();
        self.enter_scope();
        self.append_inc("then");
        Ok(false)
    }

    fn begin_else_if(&mut self, _cond: &Expr) -> WriteResult<bool> {
        self.leave_scope();
        self.append_space("elseif");
        Ok(false)
    }

    fn begin_else(&mut self, _block: &Block) -> WriteResult<bool> {
        self.leave_scope();
        self.append("else");
        self.enter_scope();
        self.incline();
        Ok(false)
    }

    fn end_if(&mut self) {
        self.end();
    }

    fn begin_while(&mut self, _cond: &Expr) -> WriteResult<bool> {
        self.append_space("while");
        Ok(false)
    }

    fn begin_while_block(&mut self, _block: &Block) -> WriteResult<bool> {
        self.enter_scope();
        self.space();
        self.append_inc("do");
        Ok(false)
    }

    fn end_while(&mut self) {
        self.end();
    }

    fn begin_do_block(&mut self, _block: &Block) -> WriteResult<bool> {
        self.enter_scope();
        self.space();
        self.append_inc("do");
        Ok(false)
    }

    fn end_do_block(&mut self) {
        self.end();
    }

    fn begin_for_num(&mut self, _for_enum: &ForNum) -> WriteResult<bool> {
        self.append_space("for");
        Ok(false)
    }

    fn for_enum_equal(&mut self) {
        self.space_append_space("=");
    }

    fn begin_for_list(&mut self, _: &ForList) -> WriteResult<bool> {
        self.append_space("for");
        Ok(false)
    }

    fn for_list_in(&mut self) {
        self.space_append_space("in");
    }

    fn begin_for_block(&mut self, _block: &Block) -> WriteResult<bool> {
        self.enter_scope();
        self.space();
        self.append_inc("do");
        Ok(false)
    }

    fn end_for(&mut self) {
        self.end();
    }

    fn begin_repeat(&mut self, _block: &Block) -> WriteResult<bool> {
        self.enter_scope();
        self.append_inc("repeat");
        Ok(false)
    }

    fn until(&mut self) {
        self.leave_scope();
        self.incline();
        self.append_space("until");
    }

    fn end_repeat(&mut self) {
        self.incline();
    }

    fn func(&mut self, funcstat: &FuncStat) {
        match funcstat.func_type {
            FuncType::Local(_) => {
                self.append_space("local");
                self.comments(&funcstat.function);
                self.append_space("function");
            }
            FuncType::Global => self.append_space("function"),
        };
        let func_name = &funcstat.func_name;
        let mut fields = func_name.fields.vars.iter();
        if let Some(name) = fields.next() {
            self.comments(name);
            self.append(&name.value());
            let remain_fields = fields.zip(func_name.fields.delimiters.iter());
            for (name, delimiter) in remain_fields {
                self.comments(delimiter);
                self.append(".");
                self.comments(name);
                self.append(&name.value());
            }
            if let Some((token, method)) = &func_name.method {
                self.comments(token);
                self.append(":");
                self.comments(method);
                self.append(&method.value());
            }
        }
    }

    fn local_stat(&mut self, stat: &LocalStat) -> WriteSuccess {
        self.append_space("local");
        for (n, name) in stat.names.vars.iter().enumerate() {
            self.comments(name);
            self.append(&name.value());
            if n < stat.names.vars.len() - 1 {
                self.comments(&stat.names.delimiters[n]);
                self.append_space(",");
            }
        }
        self.space();
        if let Some(token) = &stat.equal {
            self.comments(token);
            self.append_space("=");
            self.exprlist(stat.exprs.as_ref().unwrap())?;
        }
        Ok(())
    }

    fn label_stat(&mut self, stat: &LabelStat) -> WriteSuccess {
        self.append(&format!("::{}::", stat.label.value()));
        Ok(())
    }

    fn ret_stat(&mut self, stat: &RetStat) -> WriteSuccess {
        self.append_space("return");
        self.exprlist(stat.exprs.as_ref().unwrap())?;
        Ok(())
    }

    fn break_stat(&mut self, _stat: &BreakStat) -> WriteSuccess {
        self.append("break");
        Ok(())
    }

    fn goto_stat(&mut self, stat: &GotoStat) -> WriteSuccess {
        self.append(&format!("goto {}", stat.label.value()));
        Ok(())
    }

    fn assign_stat(&mut self, stat: &AssignStat) -> WriteSuccess {
        for (n, suffix) in stat.left.assignables.iter().enumerate() {
            ast_walker::walk_assinable(suffix, self)?;
            if n < stat.left.assignables.len() - 1 {
                self.append_space(",");
            }
        }
        self.space_append_space("=");
        self.exprlist(&stat.right)?;
        Ok(())
    }

    fn call_stat(&mut self, stat: &CallStat) -> WriteSuccess {
        ast_walker::walk_assinable(&stat.call, self)?;
        Ok(())
    }

    fn exprlist(&mut self, exprs: &ExprList) -> WriteSuccess {
        for (n, expr) in exprs.exprs.iter().enumerate() {
            if expr.has_comments() {
                self.incline_if_not_line_start();
                self.comments(expr);
            } else if n > 0 {
                self.space();
            }
            ast_walker::walk_expr(expr, self)?;
            if n < exprs.exprs.len() - 1 {
                self.append(",");
            }
        }
        Ok(())
    }

    fn nil(&mut self) {
        self.append("nil");
    }

    fn true_(&mut self) {
        self.append("true");
    }

    fn false_(&mut self) {
        self.append("false");
    }

    fn float(&mut self, f: &FloatExpr) {
        let string = if f.value().fract() == 0.0 {
            format!("{}.0", f.value())
        } else {
            f.value().to_string()
        };
        self.append(&string);
    }

    fn int(&mut self, i: &IntExpr) {
        self.append(&i.value().to_string());
    }

    fn string(&mut self, s: &StringExpr) {
        self.append(&s.value());
    }

    fn vararg(&mut self) {
        self.append("...");
    }

    fn anonymous_func(&mut self) {
        self.append_space("function");
    }

    fn begin_func_body(&mut self, body: &FuncBody) -> WriteResult<bool> {
        self.append("(");
        let has_comments = body.params.params.iter().any(|param| param.has_comments())
            || body.params.commas.iter().any(|comma| comma.has_comments());
        if has_comments {
            self.enter_scope();
            self.incline();
        }
        for (n, param) in body.params.params.iter().enumerate() {
            if param.has_comments() {
                self.incline_if_not_line_start();
            } else if n > 0 {
                self.space();
            }
            match param {
                Param::VarArg(token) => {
                    self.comments(token);
                    self.append("...")
                }
                Param::Name(s) => {
                    self.comments(s);
                    self.append(&s.value());
                }
            }
            if n < body.params.params.len() - 1 {
                self.comments(&body.params.commas[n]);
                self.append(",");
            }
        }
        self.comments(&body.rp);
        if has_comments {
            self.incline();
            self.leave_scope();
        };
        self.append_inc(")");
        self.enter_scope();
        Ok(false)
    }

    fn end_func_body(&mut self) {
        self.end();
    }

    fn begin_table(&mut self, t: &Table) -> WriteResult<bool> {
        if !t.fields.is_empty() {
            self.enter_scope();
            self.append_inc("{");
        } else {
            self.append("{}");
        }
        Ok(false)
    }

    fn end_table(&mut self, t: &Table) {
        if !t.fields.is_empty() {
            self.leave_scope();
            self.append("}");
        }
    }

    fn field_sep(&mut self) {
        self.append_inc(",");
    }

    fn field_kv_sep(&mut self) {
        self.space_append_space("=");
    }

    fn begin_field_key(&mut self, key: &FieldKey) -> WriteResult<bool> {
        if let FieldKey::Expr(..) = key {
            self.append_space("[");
        }
        Ok(false)
    }

    fn end_field_key(&mut self, key: &FieldKey) {
        if let FieldKey::Expr(..) = key {
            self.space_append("]")
        }
    }

    fn binop(&mut self, op: &BinOp) {
        let string = match op {
            BinOp::Or(_) => "or",
            BinOp::And(_) => "and",
            BinOp::Eq(_) => "==",
            BinOp::Ne(_) => "~=",
            BinOp::Lt(_) => "<",
            BinOp::Gt(_) => ">",
            BinOp::Le(_) => "<=",
            BinOp::Ge(_) => ">=",
            BinOp::BOr(_) => "|",
            BinOp::BXor(_) => "~",
            BinOp::BAnd(_) => "&",
            BinOp::Shl(_) => "<<",
            BinOp::Shr(_) => ">>",
            BinOp::Concat(_) => "..",
            BinOp::Add(_) => "+",
            BinOp::Minus(_) => "-",
            BinOp::Mul(_) => "*",
            BinOp::Mod(_) => "%",
            BinOp::Div(_) => "/",
            BinOp::IDiv(_) => "//",
            BinOp::Pow(_) => "^",
            _ => unreachable!(),
        };
        self.space_append_space(string);
    }

    fn unop(&mut self, op: &UnOp) {
        match op {
            UnOp::Minus(_) => self.append("-"),
            UnOp::BNot(_) => self.append("~"),
            UnOp::Not(_) => self.append_space("not"),
            UnOp::Len(_) => self.append("#"),
            _ => unreachable!(),
        }
    }

    fn name(&mut self, name: &StringExpr) {
        self.append(&name.value());
    }

    fn attr(&mut self, attr: &StringExpr) {
        self.append(".");
        self.append(&attr.value());
    }

    fn method(&mut self, method: &StringExpr) {
        self.append(":");
        self.append(&method.value());
    }

    fn begin_index(&mut self, _expr: &Expr) -> WriteResult<bool> {
        self.append("[");
        Ok(false)
    }

    fn end_index(&mut self) {
        self.append("]");
    }

    fn begin_func_args(&mut self, args: &FuncArgs) -> WriteResult<bool> {
        self.append("(");
        if let FuncArgs::Exprs(_, exprs, _) = args {
            if exprs.has_comments() {
                self.func_call_stack.push(FuncCallType::WithComments);
                self.incline();
                self.enter_scope();
                return Ok(false);
            }
        }
        self.func_call_stack.push(FuncCallType::WithOutComments);
        Ok(false)
    }

    fn end_func_args(&mut self) {
        if let Some(call_type) = self.func_call_stack.pop() {
            if call_type == FuncCallType::WithComments {
                self.leave_scope();
                self.incline_if_not_line_start();
            }
        }
        self.append(")");
    }

    fn begin_paren_expr(&mut self, _expr: &Expr) -> WriteResult<bool> {
        self.append("(");
        Ok(false)
    }

    fn end_paren_expr(&mut self) {
        self.append(")");
    }

    fn expr_sep(&mut self) {
        self.append(", ");
    }

    fn comments(&mut self, target: &impl Comments) {
        if self.comment_guard {
            return;
        }
        let comments = target.get_comments();
        if let Some(last_char) = self.output.chars().last() {
            if !comments.is_empty() && last_char != ' ' && last_char != '\n' {
                self.append(" ");
            }
        }
        comments
            .iter()
            .for_each(|comment| self.append_and_incline(&format!("--{}", comment)));
        self.comment_guard = !comments.is_empty();
    }
}

fn try_convert(input: &str) -> String {
    let mut lexer = Lexer::default();
    lexer.set_config(LexerConfig {
        use_origin_string: true,
        reserve_comments: true,
    });
    if let Ok(tokens) = lexer.run(input) {
        let mut parser = Parser::default();
        if let Ok(ast) = parser.run(tokens) {
            let mut writter = LuaWriter::new();
            return writter.run(&ast).to_string();
        }
    }
    unreachable!()
}

fn convert_lua(src: &str, dst: &str) -> std::io::Result<()> {
    let mut file = File::open(src)?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;

    let output = try_convert(&content);
    let mut file = File::create(dst)?;
    file.write_all(output.as_bytes())?;
    Ok(())
}

use std::process::Command;
fn exists_lua_bin() -> Option<String> {
    if Command::new("lua").output().is_ok() {
        Some("lua".to_string())
    } else if Command::new("lua5.3").output().is_ok() {
        Some("lua5.3".to_string())
    } else {
        None
    }
}

fn execute_lua_tests(bin: &str, dir: &str) -> String {
    let output = Command::new(bin)
        .current_dir(dir)
        .arg("test_all.lua")
        .output();

    let s = str::from_utf8(&output.ok().unwrap().stdout)
        .ok()
        .unwrap()
        .to_string();

    println!("{}", s);
    s
}

#[test]
fn write_method_call() {
    assert_eq!("str:sub(i, i)\n".to_string(), try_convert("str:sub(i,i)"));
}

#[test]
fn parse_comments_simple() {
    let code = "--Hello
local a = 1
local b = 2
--World
local c = 3
";
    let result = try_convert(code);
    println!("{}", result);
    assert_eq!(result, code);
}

#[test]
fn parse_function_definition_comments() {
    let code = "local function abc.d.e:f(
  -- a comment
  a,
  -- b comment
  b,
  -- c comment
  c
)
end
";
    let result = try_convert(code);
    println!("{}", result);
    assert_eq!(code, result);
}

#[test]
fn parse_stat_comment() {
    let code = "val, i = parse(str, i)
-- Set
res[key] = val
";
    let result = try_convert(code);
    println!("{}", result);
    assert_eq!(code, result);
}

#[test]
fn parse_function_call_comment() {
    let code = "foo(
  -- first argument
  a,
  -- second argument
  b,
  -- last argument
  c
)
";
    let result = try_convert(code);
    println!("{}", result);
    assert_eq!(code, result);
}

#[test]
fn parse_table_field() {
    let code = "local t = {
  -- a comment
  a = 1,
  -- b comment
  b = 2,
  -- c comment
  c = 3,
  -- array commment
  1,
  2,
  3,
}
";
    let result = try_convert(code);
    println!("{}", result);
    assert_eq!(code, result);
}

#[test]
fn lua_to_lua() -> std::io::Result<()> {
    let lua_dir: &'static str = "./lua";
    let tmp: &'static str = "./tmp";
    if let Err(_e) = read_dir(tmp) {
        create_dir(tmp)?
    }

    // convert lua files in `lua` folder
    for entry in read_dir(lua_dir)? {
        let entry = entry?;
        let file_name = entry.file_name();
        let name = file_name.to_str().unwrap();
        let src = format!("{}/{}", lua_dir, name);
        let dst = format!("{}/{}", tmp, name);
        println!("{}, {}", src, dst);
        convert_lua(&src, &dst)?;
    }

    if let Some(bin) = exists_lua_bin() {
        // execute lua sources from origin paths and temp paths, then compare their outputs
        assert_eq!(
            execute_lua_tests(&bin, "lua"),
            execute_lua_tests(&bin, "tmp")
        );
    }

    Ok(())
}
