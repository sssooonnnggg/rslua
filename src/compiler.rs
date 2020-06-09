use crate::ast::Expr;
use crate::ast::*;
use crate::ast_walker::AstVisitor;
use crate::opcodes::OpCode;
use crate::types::{FloatType, IntType};

pub struct Compiler {}

impl AstVisitor for Compiler {
    fn stat_sep(&mut self) {}

    fn begin_if(&mut self, cond: &Expr) -> bool {
        false
    }

    fn then(&mut self, block: &Block) -> bool {
        false
    }

    fn begin_else_if(&mut self, cond: &Expr) -> bool {
        false
    }

    fn begin_else(&mut self, block: &Block) -> bool {
        false
    }

    fn end_if(&mut self) {}

    fn begin_while(&mut self, cond: &Expr) -> bool {
        false
    }

    fn begin_while_block(&mut self, block: &Block) -> bool {
        false
    }

    fn end_while(&mut self) {}

    fn begin_do_block(&mut self, block: &Block) -> bool {
        false
    }

    fn end_do_block(&mut self) {}

    fn for_num(&mut self, fornum: &ForNum) -> bool {
        false
    }

    fn for_list(&mut self, forlist: &ForList) -> bool {
        false
    }

    fn begin_for_block(&mut self, block: &Block) -> bool {
        false
    }

    fn end_for(&mut self) {}

    fn begin_repeat(&mut self, block: &Block) -> bool {
        false
    }

    fn until(&mut self) {}

    fn end_repeat(&mut self) {}

    fn func(&mut self, funcstat: &FuncStat) {}

    fn local_stat(&mut self, stat: &LocalStat) {}

    fn label_stat(&mut self, stat: &LabelStat) {}

    fn ret_stat(&mut self, stat: &RetStat) {}

    fn break_stat(&mut self, stat: &BreakStat) {}

    fn goto_stat(&mut self, stat: &GotoStat) {}

    fn assign_stat(&mut self, stat: &AssignStat) {}

    fn call_stat(&mut self, stat: &CallStat) {}

    fn expr(&mut self, stat: &Expr) -> bool {
        false
    }

    fn expr_sep(&mut self) {}

    fn nil(&mut self) {}

    fn true_(&mut self) {}

    fn false_(&mut self) {}

    fn float(&mut self, f: FloatType) {}

    fn int(&mut self, i: IntType) {}

    fn string(&mut self, s: &str) {}

    fn vararg(&mut self) {}

    fn anonymous_func(&mut self) {}

    fn begin_func_body(&mut self, body: &FuncBody) -> bool {
        false
    }

    fn end_func_body(&mut self) {}

    fn begin_table(&mut self, t: &Table) -> bool {
        false
    }

    fn end_table(&mut self, t: &Table) {}

    fn field_sep(&mut self) {}

    fn begin_rec_field(&mut self, field: &RecField) -> bool {
        false
    }

    fn field_kv_sep(&mut self) {}

    fn begin_field_key(&mut self, key: &FieldKey) -> bool {
        false
    }

    fn end_field_key(&mut self, key: &FieldKey) {}

    fn end_rec_field(&mut self) {}

    fn begin_bin_expr(&mut self, expr: &BinExpr) -> bool {
        false
    }

    fn binop(&mut self, op: BinOp) {}

    fn end_bin_expr(&mut self) {}

    fn begin_un_expr(&mut self, expr: &UnExpr) -> bool {
        false
    }

    fn unop(&mut self, op: UnOp) {}

    fn end_un_expr(&mut self) {}

    fn begin_suffixed_expr(&mut self, expr: &SuffixedExpr) -> bool {
        false
    }

    fn end_suffixed_expr(&mut self) {}

    fn name(&mut self, name: &str) {}

    fn attr(&mut self, attr: &str) {}

    fn method(&mut self, method: &str) {}

    fn begin_index(&mut self, expr: &Expr) -> bool {
        false
    }

    fn end_index(&mut self) {}

    fn begin_func_args(&mut self, args: &FuncArgs) -> bool {
        false
    }

    fn end_func_args(&mut self) {}

    fn begin_paren_expr(&mut self, expr: &Expr) -> bool {
        false
    }

    fn end_paren_expr(&mut self) {}

    fn suffix(&mut self, suf: &Suffix) -> bool {
        false
    }
}
