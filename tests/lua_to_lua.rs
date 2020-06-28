use rslua::ast::*;
use rslua::ast_walker::*;
use rslua::lexer::{Lexer, LexerConfig};
use rslua::parser::Parser;
use rslua::types::*;
use std::fs::File;
use std::fs::{create_dir, read_dir};
use std::io::prelude::*;
use std::str;

struct LuaWritter {
    output: String,
    indent: usize,
    depth: usize,
}

#[allow(dead_code)]
impl LuaWritter {
    pub fn new() -> Self {
        LuaWritter {
            output: String::new(),
            indent: 2,
            depth: 0,
        }
    }

    pub fn run(&mut self, block: &Block) -> &str {
        self.output.clear();
        ast_walker::walk_block(block, self).unwrap();
        &self.output
    }

    fn append(&mut self, content: &str) {
        self.output.push_str(content);
    }

    fn incline(&mut self) {
        self.output.push_str("\n");
        self.output.push_str(&" ".repeat(self.depth * self.indent));
    }

    fn space(&mut self) {
        self.output.push_str(" ");
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
        for _i in 0..self.indent {
            self.output.pop();
        }
    }
}

type WriteResult<T> = Result<T, ()>;
type WriteSuccess = WriteResult<()>;

impl AstVisitor for LuaWritter {
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

    fn for_num(&mut self, fornum: &ForNum) -> WriteResult<bool> {
        self.append_space("for");
        self.append(&format!("{} = ", fornum.var));
        Ok(false)
    }

    fn for_list(&mut self, forlist: &ForList) -> WriteResult<bool> {
        self.append_space("for");
        for (n, var) in forlist.vars.iter().enumerate() {
            self.append(var);
            if n < forlist.vars.len() - 1 {
                self.append(", ");
            }
        }
        self.space_append_space("in");
        Ok(false)
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
            FuncType::Local => self.append_space("local function"),
            FuncType::Global => self.append_space("function"),
        };
        let func_name = &funcstat.func_name;
        let mut fields = func_name.fields.iter();
        if let Some(name) = fields.next() {
            self.append(name);
            while let Some(name) = fields.next() {
                self.append(".");
                self.append(name);
            }
            if let Some(method) = &func_name.method {
                self.append(":");
                self.append(method);
            }
        }
    }

    fn local_stat(&mut self, stat: &LocalStat) -> WriteSuccess {
        self.append_space("local");
        for (n, name) in stat.names.iter().enumerate() {
            self.append(name);
            if n < stat.names.len() - 1 {
                self.append(", ");
            }
        }
        self.space();
        if stat.exprs.len() > 0 {
            self.append_space("=");
            ast_walker::walk_exprlist(&stat.exprs, self)?;
        }
        Ok(())
    }

    fn label_stat(&mut self, stat: &LabelStat) -> WriteSuccess {
        self.append(&format!("::{}::", stat.label));
        Ok(())
    }

    fn ret_stat(&mut self, stat: &RetStat) -> WriteSuccess {
        self.append_space("return");
        ast_walker::walk_exprlist(&stat.exprs, self)?;
        Ok(())
    }

    fn break_stat(&mut self, _stat: &BreakStat) -> WriteSuccess {
        self.append("break");
        Ok(())
    }

    fn goto_stat(&mut self, stat: &GotoStat) -> WriteSuccess {
        self.append(&format!("goto {}", stat.label));
        Ok(())
    }

    fn assign_stat(&mut self, stat: &AssignStat) -> WriteSuccess {
        for (n, suffix) in stat.left.iter().enumerate() {
            ast_walker::walk_assinable(suffix, self)?;
            if n < stat.left.len() - 1 {
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

    fn float(&mut self, f: FloatType) {
        let string = if f.fract() == 0.0 {
            format!("{}.0", f)
        } else {
            f.to_string()
        };
        self.append(&string);
    }

    fn int(&mut self, i: IntType) {
        self.append(&i.to_string());
    }

    fn string(&mut self, s: &str) {
        self.append(s);
    }

    fn vararg(&mut self) {
        self.append("...");
    }

    fn anonymous_func(&mut self) {
        self.append_space("function");
    }

    fn begin_func_body(&mut self, body: &FuncBody) -> WriteResult<bool> {
        self.append("(");
        for (n, param) in body.params.iter().enumerate() {
            match param {
                Param::VarArg => self.append("..."),
                Param::Name(s) => self.append(s),
            }
            if n < body.params.len() - 1 {
                self.append(", ");
            }
        }
        self.enter_scope();
        self.append_inc(")");
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
            FieldKey::Expr(_) => self.append_space("["),
            _ => (),
        }
        Ok(false)
    }

    fn end_field_key(&mut self, key: &FieldKey) {
        match key {
            FieldKey::Expr(_) => self.space_append("]"),
            _ => (),
        }
    }

    fn binop(&mut self, op: BinOp) {
        let string = match op {
            BinOp::Or => "or",
            BinOp::And => "and",
            BinOp::Eq => "==",
            BinOp::Ne => "~=",
            BinOp::Lt => "<",
            BinOp::Gt => ">",
            BinOp::Le => "<=",
            BinOp::Ge => ">=",
            BinOp::BOr => "|",
            BinOp::BXor => "~",
            BinOp::BAnd => "&",
            BinOp::Shl => "<<",
            BinOp::Shr => ">>",
            BinOp::Concat => "..",
            BinOp::Add => "+",
            BinOp::Minus => "-",
            BinOp::Mul => "*",
            BinOp::Mod => "%",
            BinOp::Div => "/",
            BinOp::IDiv => "//",
            BinOp::Pow => "^",
            _ => unreachable!(),
        };
        self.space_append_space(string);
    }

    fn unop(&mut self, op: UnOp) {
        match op {
            UnOp::Minus => self.append("-"),
            UnOp::BNot => self.append("~"),
            UnOp::Not => self.append_space("not"),
            UnOp::Len => self.append("#"),
            _ => unreachable!(),
        }
    }

    fn name(&mut self, name: &str) {
        self.append(name);
    }

    fn attr(&mut self, attr: &str) {
        self.append(".");
        self.append(attr);
    }

    fn method(&mut self, method: &str) {
        self.append(":");
        self.append(method);
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

    fn comment(&mut self, comment: &CommentStat) {
        self.append(&format!("--{}", comment.comment));
    }
}

fn try_convert(input: &str) -> String {
    let mut lexer = Lexer::new();
    lexer.set_debug(true);
    lexer.set_config(LexerConfig {
        use_origin_string: true,
        reserve_comments: true,
    });
    if let Ok(tokens) = lexer.run(&input) {
        let mut parser = Parser::new();
        parser.set_debug(true);
        if let Ok(ast) = parser.run(tokens) {
            let mut writter = LuaWritter::new();
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
