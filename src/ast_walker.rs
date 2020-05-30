use crate::ast::*;
use crate::types::*;

// if visitor return `true`, walker will not travel its child nodes
pub trait AstVisitor {
    fn stat_sep(&mut self);

    fn begin_if(&mut self, cond: &Expr) -> bool;
    fn then(&mut self, block: &Block) -> bool;
    fn begin_else_if(&mut self, cond: &Expr) -> bool;
    fn begin_else(&mut self, block: &Block) -> bool;
    fn end_if(&mut self);

    fn begin_while(&mut self, cond: &Expr) -> bool;
    fn begin_while_block(&mut self, block: &Block) -> bool;
    fn end_while(&mut self);

    fn begin_do_block(&mut self, block: &Block) -> bool;
    fn end_do_block(&mut self);

    fn for_num(&mut self, fornum: &ForNum) -> bool;
    fn for_list(&mut self, forlist: &ForList) -> bool;
    fn begin_for_block(&mut self, block: &Block) -> bool;
    fn end_for(&mut self);

    fn begin_repeat(&mut self, block: &Block) -> bool;
    fn until(&mut self);
    fn end_repeat(&mut self);

    fn func(&mut self, funcstat: &FuncStat);

    fn local_stat(&mut self, stat: &LocalStat);
    fn label_stat(&mut self, stat: &LabelStat);
    fn ret_stat(&mut self, stat: &RetStat);
    fn break_stat(&mut self, stat: &BreakStat);
    fn goto_stat(&mut self, stat: &GotoStat);
    fn assign_stat(&mut self, stat: &AssignStat);
    fn call_stat(&mut self, stat: &CallStat);

    fn expr(&mut self, stat: &Expr) -> bool;
    fn expr_sep(&mut self);

    fn nil(&mut self);
    fn true_(&mut self);
    fn false_(&mut self);
    fn float(&mut self, f: FloatType);
    fn int(&mut self, i: IntType);
    fn string(&mut self, s: &str);
    fn vararg(&mut self);

    fn anonymous_func(&mut self);
    fn begin_func_body(&mut self, body: &FuncBody) -> bool;
    fn end_func_body(&mut self);

    fn begin_table(&mut self, t: &Table) -> bool;
    fn end_table(&mut self, t: &Table);

    fn field_sep(&mut self);

    fn begin_rec_field(&mut self, field: &RecField) -> bool;
    fn field_kv_sep(&mut self);
    fn begin_field_key(&mut self, key: &FieldKey) -> bool;
    fn end_field_key(&mut self, key: &FieldKey);
    fn end_rec_field(&mut self);

    fn begin_bin_expr(&mut self, expr: &BinExpr) -> bool;
    fn binop(&mut self, op: BinOp);
    fn end_bin_expr(&mut self);

    fn begin_un_expr(&mut self, expr: &UnExpr) -> bool;
    fn unop(&mut self, op: UnOp);
    fn end_un_expr(&mut self);

    fn begin_suffixed_expr(&mut self, expr: &SuffixedExpr) -> bool;
    fn end_suffixed_expr(&mut self);

    fn begin_primary_expr(&mut self, expr: &PrimaryExpr) -> bool;
    fn end_primary_expr(&mut self);

    fn name(&mut self, name: &str);
    fn attr(&mut self, attr: &str);
    fn method(&mut self, method: &str);

    fn begin_index(&mut self, expr: &Expr) -> bool;
    fn end_index(&mut self);

    fn begin_func_args(&mut self, args: &FuncArgs) -> bool;
    fn end_func_args(&mut self);

    fn begin_paren_expr(&mut self, expr: &Expr) -> bool;
    fn end_paren_expr(&mut self);

    fn suffix(&mut self, suf: &Suffix) -> bool;
}

pub mod ast_walker {
    use super::AstVisitor;
    use crate::ast::*;

    pub fn walk_block<T: AstVisitor>(block: &Block, visitor: &mut T) {
        for stat in block.stats.iter() {
            walk_stat(stat, visitor);
            visitor.stat_sep();
        }
    }

    pub fn walk_stat<T: AstVisitor>(stat: &Stat, visitor: &mut T) {
        match stat {
            Stat::Empty => (),
            Stat::IfStat(ifstat) => walk_ifstat(ifstat, visitor),
            Stat::WhileStat(whilestat) => walk_whilestat(whilestat, visitor),
            Stat::DoBlock(doblock) => walk_doblockstat(doblock, visitor),
            Stat::ForStat(forstat) => walk_forstat(forstat, visitor),
            Stat::RepeatStat(repeatstat) => walk_repeatstat(repeatstat, visitor),
            Stat::FuncStat(funcstat) => walk_funcstat(funcstat, visitor),
            Stat::LocalStat(localstat) => walk_localstat(localstat, visitor),
            Stat::LabelStat(labelstat) => walk_labelstat(labelstat, visitor),
            Stat::RetStat(retstat) => walk_retstat(retstat, visitor),
            Stat::BreakStat(breakstat) => walk_breakstat(breakstat, visitor),
            Stat::GotoStat(gotostat) => walk_gotostat(gotostat, visitor),
            Stat::AssignStat(assignstat) => walk_assignstat(assignstat, visitor),
            Stat::CallStat(callstat) => walk_callstat(callstat, visitor),
        };
    }

    pub fn walk_ifstat<T: AstVisitor>(stat: &IfStat, visitor: &mut T) {
        let mut if_blocks = stat.cond_blocks.iter();
        if let Some(if_block) = if_blocks.next() {
            if !visitor.begin_if(&if_block.cond) {
                walk_expr(&if_block.cond, visitor);
            }
            if !visitor.then(&if_block.block) {
                walk_block(&if_block.block, visitor);
            }
            while let Some(else_if_block) = if_blocks.next() {
                if !visitor.begin_else_if(&else_if_block.cond) {
                    walk_expr(&else_if_block.cond, visitor);
                }
                if !visitor.then(&else_if_block.block) {
                    walk_block(&else_if_block.block, visitor);
                }
            }
            if stat.else_block.stats.len() > 0 {
                if !visitor.begin_else(&stat.else_block) {
                    walk_block(&stat.else_block, visitor);
                }
            }
            visitor.end_if();
        }
    }

    pub fn walk_whilestat<T: AstVisitor>(stat: &WhileStat, visitor: &mut T) {
        if !visitor.begin_while(&stat.cond) {
            walk_expr(&stat.cond, visitor);
        }
        if !visitor.begin_while_block(&stat.block) {
            walk_block(&stat.block, visitor);
        }
        visitor.end_while();
    }

    pub fn walk_doblockstat<T: AstVisitor>(stat: &DoBlock, visitor: &mut T) {
        if !visitor.begin_do_block(&stat.block) {
            walk_block(&stat.block, visitor);
        }
        visitor.end_do_block();
    }

    pub fn walk_forstat<T: AstVisitor>(stat: &ForStat, visitor: &mut T) {
        match stat {
            ForStat::ForNum(fornum) => walk_forenum(fornum, visitor),
            ForStat::ForList(forlist) => walk_forlist(forlist, visitor),
        };
    }

    pub fn walk_forenum<T: AstVisitor>(stat: &ForNum, visitor: &mut T) {
        if !visitor.for_num(stat) {
            walk_expr(&stat.init, visitor);
            visitor.expr_sep();
            walk_expr(&stat.limit, visitor);
            if let Some(expr) = &stat.step {
                visitor.expr_sep();
                walk_expr(expr, visitor);
            }
        }
        if !visitor.begin_for_block(&stat.body) {
            walk_block(&stat.body, visitor);
        }
        visitor.end_for();
    }

    pub fn walk_forlist<T: AstVisitor>(stat: &ForList, visitor: &mut T) {
        if !visitor.for_list(stat) {
            walk_exprlist(&stat.exprs, visitor);
        }
        if !visitor.begin_for_block(&stat.body) {
            walk_block(&stat.body, visitor);
        }
        visitor.end_for();
    }

    pub fn walk_repeatstat<T: AstVisitor>(stat: &RepeatStat, visitor: &mut T) {
        if !visitor.begin_repeat(&stat.block) {
            walk_block(&stat.block, visitor);
            visitor.until();
            walk_expr(&stat.cond, visitor);
        }
        visitor.end_repeat();
    }

    pub fn walk_funcstat<T: AstVisitor>(stat: &FuncStat, visitor: &mut T) {
        visitor.func(stat);
        walk_funcbody(&stat.body, visitor);
    }

    pub fn walk_localstat<T: AstVisitor>(stat: &LocalStat, visitor: &mut T) {
        visitor.local_stat(stat);
    }

    pub fn walk_labelstat<T: AstVisitor>(stat: &LabelStat, visitor: &mut T) {
        visitor.label_stat(stat);
    }

    pub fn walk_retstat<T: AstVisitor>(stat: &RetStat, visitor: &mut T) {
        visitor.ret_stat(stat);
    }

    pub fn walk_breakstat<T: AstVisitor>(stat: &BreakStat, visitor: &mut T) {
        visitor.break_stat(stat);
    }

    pub fn walk_gotostat<T: AstVisitor>(stat: &GotoStat, visitor: &mut T) {
        visitor.goto_stat(stat);
    }

    pub fn walk_assignstat<T: AstVisitor>(stat: &AssignStat, visitor: &mut T) {
        visitor.assign_stat(stat);
    }

    pub fn walk_callstat<T: AstVisitor>(stat: &CallStat, visitor: &mut T) {
        visitor.call_stat(stat);
    }

    pub fn walk_expr<T: AstVisitor>(expr: &Expr, visitor: &mut T) {
        if !visitor.expr(expr) {
            match expr {
                Expr::Nil => visitor.nil(),
                Expr::True => visitor.true_(),
                Expr::False => visitor.false_(),
                Expr::Float(f) => visitor.float(*f),
                Expr::Int(i) => visitor.int(*i),
                Expr::String(string) => visitor.string(string),
                Expr::VarArg => visitor.vararg(),
                Expr::FuncBody(body) => {
                    visitor.anonymous_func();
                    walk_funcbody(body, visitor)
                }
                Expr::Table(t) => walk_table(t, visitor),
                Expr::BinExpr(expr) => walk_binexpr(expr, visitor),
                Expr::UnExpr(expr) => walk_unexpr(expr, visitor),
                Expr::SuffixedExpr(expr) => walk_suffixedexpr(expr, visitor),
            };
        }
    }

    pub fn walk_funcbody<T: AstVisitor>(body: &FuncBody, visitor: &mut T) {
        if !visitor.begin_func_body(body) {
            walk_block(&body.block, visitor);
        }
        visitor.end_func_body();
    }

    pub fn walk_binexpr<T: AstVisitor>(expr: &BinExpr, visitor: &mut T) {
        if !visitor.begin_bin_expr(expr) {
            walk_expr(&expr.left, visitor);
            visitor.binop(expr.op);
            walk_expr(&expr.right, visitor);
        }
        visitor.end_bin_expr();
    }

    pub fn walk_unexpr<T: AstVisitor>(expr: &UnExpr, visitor: &mut T) {
        if !visitor.begin_un_expr(expr) {
            visitor.unop(expr.op);
            walk_expr(&expr.expr, visitor);
        }
        visitor.end_un_expr();
    }

    pub fn walk_suffixedexpr<T: AstVisitor>(expr: &SuffixedExpr, visitor: &mut T) {
        if !visitor.begin_suffixed_expr(expr) {
            walk_primaryexpr(&expr.primary, visitor);
            for suf in expr.suffixes.iter() {
                if !visitor.suffix(suf) {
                    match suf {
                        Suffix::Attr(attr) => visitor.attr(attr),
                        Suffix::Method(method) => visitor.method(method),
                        Suffix::Index(index) => walk_index(index, visitor),
                        Suffix::FuncArgs(args) => walk_funcargs(args, visitor),
                    }
                }
            }
        }
        visitor.end_suffixed_expr();
    }

    pub fn walk_index<T: AstVisitor>(expr: &Expr, visitor: &mut T) {
        if !visitor.begin_index(expr) {
            walk_expr(expr, visitor);
        }
        visitor.end_index();
    }

    pub fn walk_funcargs<T: AstVisitor>(args: &FuncArgs, visitor: &mut T) {
        if !visitor.begin_func_args(args) {
            match args {
                FuncArgs::String(s) => visitor.string(s),
                FuncArgs::Table(t) => walk_table(t, visitor),
                FuncArgs::Exprs(exprs) => walk_exprlist(exprs, visitor),
            }
        }
        visitor.end_func_args();
    }

    pub fn walk_primaryexpr<T: AstVisitor>(expr: &PrimaryExpr, visitor: &mut T) {
        if !visitor.begin_primary_expr(expr) {
            match expr {
                PrimaryExpr::Name(n) => visitor.name(n),
                PrimaryExpr::ParenExpr(expr) => walk_parenexpr(&expr, visitor),
            }
        }
        visitor.end_primary_expr();
    }

    pub fn walk_parenexpr<T: AstVisitor>(expr: &Expr, visitor: &mut T) {
        if !visitor.begin_paren_expr(expr) {
            walk_expr(expr, visitor);
        }
        visitor.end_paren_expr();
    }

    pub fn walk_table<T: AstVisitor>(table: &Table, visitor: &mut T) {
        if !visitor.begin_table(table) {
            walk_fields(&table.fields, visitor);
        }
        visitor.end_table(table);
    }

    pub fn walk_fields<T: AstVisitor>(fields: &Vec<Field>, visitor: &mut T) {
        for field in fields.iter() {
            walk_field(field, visitor);
            visitor.field_sep();
        }
    }

    pub fn walk_field<T: AstVisitor>(field: &Field, visitor: &mut T) {
        match field {
            Field::RecFileld(field) => walk_recfield(field, visitor),
            Field::ListField(field) => walk_expr(field, visitor),
        };
    }

    pub fn walk_recfield<T: AstVisitor>(field: &RecField, visitor: &mut T) {
        if !visitor.begin_rec_field(field) {
            walk_fieldkey(&field.key, visitor);
            visitor.field_kv_sep();
            walk_expr(&field.value, visitor);
        }
        visitor.end_rec_field();
    }

    pub fn walk_fieldkey<T: AstVisitor>(key: &FieldKey, visitor: &mut T) {
        if !visitor.begin_field_key(key) {
            match key {
                FieldKey::Name(s) => visitor.name(s),
                FieldKey::Expr(expr) => walk_expr(expr, visitor),
            };
        }
        visitor.end_field_key(key);
    }

    pub fn walk_exprlist<T: AstVisitor>(exprlist: &Vec<Expr>, visitor: &mut T) {
        for (n, expr) in exprlist.iter().enumerate() {
            walk_expr(expr, visitor);
            if n < exprlist.len() - 1 {
                visitor.expr_sep();
            }
        }
    }
}
