use std::str;
use rua::ast::*;
use rua::ast_walker::*;
use rua::lexer::Lexer;
use rua::parser::Parser;
use rua::types::*;
use std::fs::File;
use std::io::prelude::*;

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
        ast_walker::walk_block(block, self);
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

impl AstVisitor for LuaWritter {
    fn stat_sep(&mut self) {
        self.incline();
    }

    fn begin_if(&mut self, _cond: &Expr) -> bool {
        self.append_space("if");
        false
    }

    fn then(&mut self, _block: &Block) -> bool {
        self.space();
        self.enter_scope();
        self.append_inc("then");
        false
    }

    fn begin_else_if(&mut self, _cond: &Expr) -> bool {
        self.leave_scope();
        self.append_space("elseif");
        false
    }

    fn begin_else(&mut self, _block: &Block) -> bool {
        self.leave_scope();
        self.append("else");
        self.enter_scope();
        self.incline();
        false
    }

    fn end_if(&mut self) {
        self.end();
    }

    fn begin_while(&mut self, _cond: &Expr) -> bool {
        self.append_space("while");
        false
    }

    fn begin_while_block(&mut self, _block: &Block) -> bool {
        self.enter_scope();
        self.space();
        self.append_inc("do");
        false
    }

    fn end_while(&mut self) {
        self.end();
    }

    fn begin_do_block(&mut self, _block: &Block) -> bool {
        self.enter_scope();
        self.space();
        self.append_inc("do");
        false
    }

    fn end_do_block(&mut self) {
        self.end();
    }

    fn for_num(&mut self, fornum: &ForNum) -> bool {
        self.append_space("for");
        self.append(&format!("{} = ", fornum.var));
        false
    }

    fn for_list(&mut self, forlist: &ForList) -> bool {
        self.append_space("for");
        for (n, var) in forlist.vars.iter().enumerate() {
            self.append(var);
            if n < forlist.vars.len() - 1 {
                self.append(", ");
            }
        }
        self.space();
        self.append_space("in");
        false
    }

    fn begin_for_block(&mut self, _block: &Block) -> bool {
        self.enter_scope();
        self.space();
        self.append_inc("do");
        false
    }

    fn end_for(&mut self) {
        self.end();
    }

    fn begin_repeat(&mut self, _block: &Block) -> bool {
        self.enter_scope();
        self.append_inc("repeat");
        false
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

    fn local_stat(&mut self, stat: &LocalStat) {
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
            ast_walker::walk_exprlist(&stat.exprs, self);
        }
    }

    fn label_stat(&mut self, stat: &LabelStat) {
        self.append(&format!("::{}::", stat.label));
    }

    fn ret_stat(&mut self, stat: &RetStat) {
        self.append_space("return");
        ast_walker::walk_exprlist(&stat.exprs, self);
    }

    fn break_stat(&mut self, _stat: &BreakStat) {
        self.append("break");
    }

    fn goto_stat(&mut self, stat: &GotoStat) {
        self.append(&format!("goto {}", stat.label));
    }

    fn assign_stat(&mut self, stat: &AssignStat) {
        for (n, suffix) in stat.left.iter().enumerate() {
            ast_walker::walk_suffixedexpr(suffix, self);
            if n < stat.left.len() - 1 {
                self.append(", ");
            }
        }
        self.space();
        self.append_space("=");
        ast_walker::walk_exprlist(&stat.right, self);
    }

    fn call_stat(&mut self, stat: &CallStat) {
        ast_walker::walk_suffixedexpr(&stat.call, self);
    }

    fn expr(&mut self, _stat: &Expr) -> bool {
        false
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
        self.append(&format!("{}", f));
    }

    fn int(&mut self, i: IntType) {
        self.append(&format!("{}", i));
    }

    fn string(&mut self, s: &str) {
        let mut new_str:Vec<u8> = Vec::new();
        new_str.push(b'"');
        let bytes = s.as_bytes();
        for i in 0..bytes.len() {
            match bytes[i] {
                b'\\' => new_str.extend_from_slice("\\\\".as_bytes()),
                b'"' => new_str.extend_from_slice("\\\"".as_bytes()),
                b'\n' => new_str.extend_from_slice("\\n".as_bytes()),
                b'\r' => new_str.extend_from_slice("\\r".as_bytes()),
                b'\t' => new_str.extend_from_slice("\\t".as_bytes()),
                b'\x07' => new_str.extend_from_slice("\\a".as_bytes()),
                b'\x08' => new_str.extend_from_slice("\\b".as_bytes()),
                b'\x0B' => new_str.extend_from_slice("\\v".as_bytes()),
                b'\x0C' => new_str.extend_from_slice("\\f".as_bytes()),
                _ => new_str.push(bytes[i]),
            }
        }
        new_str.push(b'"');
        if let Ok(s) = str::from_utf8(&new_str) {
            self.append(s);
        }
    }

    fn vararg(&mut self) {
        self.append("...");
    }

    fn anonymous_func(&mut self) {
        self.append_space("function");
    }

    fn begin_func_body(&mut self, body: &FuncBody) -> bool {
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
        false
    }

    fn end_func_body(&mut self) {
        self.end();
    }

    fn begin_table(&mut self, t: &Table) -> bool {
        if t.fields.len() > 0 {
            self.enter_scope();
            self.append_inc("{");
        } else {
            self.append("{}");
        }

        false
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

    fn begin_rec_field(&mut self, _field: &RecField) -> bool {
        false
    }

    fn field_kv_sep(&mut self) {
        self.space();
        self.append_space("=");
    }

    fn begin_field_key(&mut self, key: &FieldKey) -> bool {
        match key {
            FieldKey::Expr(_) => self.append("["),
            _ => (),
        }
        false
    }

    fn end_field_key(&mut self, key: &FieldKey) {
        match key {
            FieldKey::Expr(_) => self.append("]"),
            _ => (),
        }
    }

    fn end_rec_field(&mut self) {}

    fn begin_bin_expr(&mut self, _expr: &BinExpr) -> bool {
        false
    }

    fn binop(&mut self, op: BinOp) {
        self.space();
        match op {
            BinOp::Or => self.append_space("or"),
            BinOp::And => self.append_space("and"),
            BinOp::Eq => self.append_space("or"),
            BinOp::Ne => self.append_space("~="),
            BinOp::Lt => self.append_space("<"),
            BinOp::Gt => self.append_space(">"),
            BinOp::Le => self.append_space("<="),
            BinOp::Ge => self.append_space(">="),
            BinOp::BOr => self.append_space("|"),
            BinOp::BXor => self.append_space("~"),
            BinOp::BAnd => self.append_space("&"),
            BinOp::Shl => self.append_space("<<"),
            BinOp::Shr => self.append_space(">>"),
            BinOp::Concat => self.append_space(".."),
            BinOp::Add => self.append_space("+"),
            BinOp::Minus => self.append_space("-"),
            BinOp::Mul => self.append_space("*"),
            BinOp::Mod => self.append_space("%"),
            BinOp::Div => self.append_space("/"),
            BinOp::IDiv => self.append_space("//"),
            BinOp::Pow => self.append_space("^"),
            _ => unreachable!(),
        };
    }

    fn end_bin_expr(&mut self) {}

    fn begin_un_expr(&mut self, _expr: &UnExpr) -> bool {
        false
    }

    fn unop(&mut self, op: UnOp) {
        match op {
            UnOp::Minus => self.append("-"),
            UnOp::BNot => self.append("~"),
            UnOp::Not => self.append("not"),
            UnOp::TLen => self.append("#"),
            _ => unreachable!(),
        }
    }

    fn end_un_expr(&mut self) {}

    fn begin_suffixed_expr(&mut self, _expr: &SuffixedExpr) -> bool {
        false
    }

    fn end_suffixed_expr(&mut self) {}

    fn begin_primary_expr(&mut self, _expr: &PrimaryExpr) -> bool {
        false
    }

    fn end_primary_expr(&mut self) {}

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

    fn begin_index(&mut self, _expr: &Expr) -> bool {
        self.append("[");
        false
    }

    fn end_index(&mut self) {
        self.append("]");
    }

    fn begin_func_args(&mut self, _args: &FuncArgs) -> bool {
        self.append("(");
        false
    }

    fn end_func_args(&mut self) {
        self.append(")");
    }

    fn begin_paren_expr(&mut self, _expr: &Expr) -> bool {
        self.append("(");
        false
    }

    fn end_paren_expr(&mut self) {
        self.append(")");
    }

    fn suffix(&mut self, _suf: &Suffix) -> bool {
        false
    }
}

fn try_convert(input: &str) -> String {
    let mut lexer = Lexer::new();
    lexer.set_debug(true);
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

#[test]
fn write_method_call() {
    assert_eq!(
        "str:sub(i, i)\n".to_string(),
        try_convert("str:sub(i,i)")
    );
}

#[test]
fn lua_to_lua() -> std::io::Result<()> {
    let mut file = File::open(r"lua/json.lua")?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;

    let output = try_convert(&content);
    let mut file = File::create("tests/json_output.lua")?;
    file.write_all(output.as_bytes())?;
    Ok(())
}
