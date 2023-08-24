use rslua::ast::*;
use rslua::ast_walker::*;
use rslua::lexer::{Lexer, LexerConfig};
use rslua::parser::Parser;
use rslua_traits::Comments;
use std::fs::File;
use std::fs::{create_dir, read_dir};
use std::io::prelude::*;
use std::str;

struct LuaWriter {
    output: String,
    indent: usize,
    depth: usize,
    line_start: bool,
}

#[allow(dead_code)]
impl LuaWriter {
    pub fn new() -> Self {
        LuaWriter {
            output: String::new(),
            indent: 2,
            depth: 0,
            line_start: true,
        }
    }

    pub fn run(&mut self, block: &Block) -> &str {
        self.output.clear();
        ast_walker::walk_block(block, self).unwrap();
        &self.output
    }

    fn append(&mut self, content: &str) {
        self.indent_at_line_start();
        self.output.push_str(content);
    }

    fn indent_at_line_start(&mut self) {
        if self.line_start {
            self.output.push_str(&" ".repeat(self.depth * self.indent));
            self.line_start = false;
        }
    }

    fn incline(&mut self) {
        self.output.push_str("\n");
        self.line_start = true;
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

    fn begin_for_list(&mut self, forlist: &ForList) -> WriteResult<bool> {
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
            let mut remain_fields = fields.zip(func_name.fields.delimiters.iter());
            while let Some((name, delimiter)) = remain_fields.next() {
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
            self.append(&name.value());
            if n < stat.names.vars.len() - 1 {
                self.append(", ");
            }
        }
        self.space();
        if let Some(_) = stat.equal {
            self.append_space("=");
            ast_walker::walk_exprlist(stat.exprs.as_ref().unwrap(), self)?;
        }
        Ok(())
    }

    fn label_stat(&mut self, stat: &LabelStat) -> WriteSuccess {
        self.append(&format!("::{}::", stat.label.value()));
        Ok(())
    }

    fn ret_stat(&mut self, stat: &RetStat) -> WriteSuccess {
        self.append_space("return");
        ast_walker::walk_exprlist(stat.exprs.as_ref().unwrap(), self)?;
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
        ast_walker::walk_exprlist(&stat.right, self)?;
        Ok(())
    }

    fn call_stat(&mut self, stat: &CallStat) -> WriteSuccess {
        ast_walker::walk_assinable(&stat.call, self)?;
        Ok(())
    }

    fn expr_sep(&mut self) {
        self.append(", ");
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
                self.append(", ");
            }
        }
        self.comments(&body.rp);
        if has_comments {
            self.leave_scope()
        };
        self.append_inc(")");
        self.enter_scope();
        Ok(false)
    }

    fn end_func_body(&mut self) {
        self.end();
    }

    fn begin_table(&mut self, t: &Table) -> WriteResult<bool> {
        if t.fields.len() > 0 {
            self.enter_scope();
            self.append_inc("{");
        } else {
            self.append("{}");
        }
        Ok(false)
    }

    fn end_table(&mut self, t: &Table) {
        if t.fields.len() > 0 {
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
        match key {
            FieldKey::Expr(_, _, _) => self.append_space("["),
            _ => (),
        }
        Ok(false)
    }

    fn end_field_key(&mut self, key: &FieldKey) {
        match key {
            FieldKey::Expr(_, _, _) => self.space_append("]"),
            _ => (),
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

    fn begin_func_args(&mut self, _args: &FuncArgs) -> WriteResult<bool> {
        self.append("(");
        Ok(false)
    }

    fn end_func_args(&mut self) {
        self.append(")");
    }

    fn begin_paren_expr(&mut self, _expr: &Expr) -> WriteResult<bool> {
        self.append("(");
        Ok(false)
    }

    fn end_paren_expr(&mut self) {
        self.append(")");
    }

    fn comments(&mut self, comments: &impl Comments) {
        let comments = comments.get_comments();
        if let Some(last_char) = self.output.chars().last() && !comments.is_empty(){
            if last_char != ' ' && last_char != '\n'{
                self.append(" ")
            }
        }
        for comment in comments {
            self.append_and_incline(&format!("--{}", comment));
        }
    }
}

fn try_convert(input: &str) -> String {
    let mut lexer = Lexer::new();
    lexer.set_config(LexerConfig {
        use_origin_string: true,
        reserve_comments: true,
    });
    if let Ok(tokens) = lexer.run(&input) {
        let mut parser = Parser::new();
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
    if let Ok(_) = Command::new("lua").output() {
        Some("lua".to_string())
    } else if let Ok(_) = Command::new("lua5.3").output() {
        Some("lua5.3".to_string())
    } else {
        None
    }
}

fn execute_lua_tests(bin: &str, dir: &str) -> String {
    let output = Command::new(&bin)
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
fn parse_function_comments() {
    let code = "local 
-- comment1
function 
-- comment2
abc.d.e:f(
    a, -- a comment
    b, -- b comment
    c -- c comment
)
end
";
    let result = try_convert(code);
    // assert_eq!(code, result);
    println!("{}", result);
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
