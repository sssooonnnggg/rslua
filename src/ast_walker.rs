use rslua_traits::Comments;

use crate::ast::*;
use crate::types::*;

// if visitor return `Ok(true)`, walker will not travel its children
// if visitor return `Ok(false)`, walker will travel its children recursively.
// if visitor return `Err(E)`, walker will stop traveling.
pub trait AstVisitor<E = ()> {
    fn stat_sep(&mut self) {}

    fn begin_if(&mut self, _cond: &Expr) -> Result<bool, E> {
        Ok(false)
    }
    fn then(&mut self, _block: &Block) -> Result<bool, E> {
        Ok(false)
    }
    fn begin_else_if(&mut self, _cond: &Expr) -> Result<bool, E> {
        Ok(false)
    }
    fn begin_else(&mut self, _block: &Block) -> Result<bool, E> {
        Ok(false)
    }
    fn end_if(&mut self) {}

    fn begin_while(&mut self, _cond: &Expr) -> Result<bool, E> {
        Ok(false)
    }
    fn begin_while_block(&mut self, _block: &Block) -> Result<bool, E> {
        Ok(false)
    }
    fn end_while(&mut self) {}

    fn begin_do_block(&mut self, _block: &Block) -> Result<bool, E> {
        Ok(false)
    }
    fn end_do_block(&mut self) {}

    fn begin_for_num(&mut self, _for_enum: &ForNum) -> Result<bool, E> {
        Ok(false)
    }
    fn for_enum_equal(&mut self) {}

    fn begin_for_list(&mut self, _forlist: &ForList) -> Result<bool, E> {
        Ok(false)
    }

    fn for_list_in(&mut self) {}

    fn begin_for_block(&mut self, _block: &Block) -> Result<bool, E> {
        Ok(false)
    }
    fn end_for(&mut self) {}

    fn begin_repeat(&mut self, _block: &Block) -> Result<bool, E> {
        Ok(false)
    }
    fn until(&mut self) {}
    fn end_repeat(&mut self) {}

    fn func(&mut self, _funcstat: &FuncStat) {}

    fn local_stat(&mut self, _stat: &LocalStat) -> Result<(), E> {
        Ok(())
    }
    fn label_stat(&mut self, _stat: &LabelStat) -> Result<(), E> {
        Ok(())
    }
    fn ret_stat(&mut self, _stat: &RetStat) -> Result<(), E> {
        Ok(())
    }
    fn break_stat(&mut self, _stat: &BreakStat) -> Result<(), E> {
        Ok(())
    }
    fn goto_stat(&mut self, _stat: &GotoStat) -> Result<(), E> {
        Ok(())
    }
    fn assign_stat(&mut self, _stat: &AssignStat) -> Result<(), E> {
        Ok(())
    }
    fn call_stat(&mut self, _stat: &CallStat) -> Result<(), E> {
        Ok(())
    }

    fn expr(&mut self, _stat: &Expr) -> Result<bool, E> {
        Ok(false)
    }
    fn expr_sep(&mut self) {}

    fn nil(&mut self) {}
    fn true_(&mut self) {}
    fn false_(&mut self) {}
    fn float(&mut self, _f: &FloatExpr) {}
    fn int(&mut self, _i: &IntExpr) {}
    fn string(&mut self, _s: &StringExpr) {}
    fn vararg(&mut self) {}

    fn anonymous_func(&mut self) {}
    fn begin_func_body(&mut self, _body: &FuncBody) -> Result<bool, E> {
        Ok(false)
    }
    fn end_func_body(&mut self) {}

    fn begin_table(&mut self, _t: &Table) -> Result<bool, E> {
        Ok(false)
    }
    fn end_table(&mut self, _t: &Table) {}

    fn field_sep(&mut self) {}

    fn begin_rec_field(&mut self, _field: &RecField) -> Result<bool, E> {
        Ok(false)
    }
    fn field_kv_sep(&mut self) {}
    fn begin_field_key(&mut self, _key: &FieldKey) -> Result<bool, E> {
        Ok(false)
    }
    fn end_field_key(&mut self, _key: &FieldKey) {}
    fn end_rec_field(&mut self) {}

    fn begin_bin_expr(&mut self, _expr: &BinExpr) -> Result<bool, E> {
        Ok(false)
    }
    fn binop(&mut self, _op: &BinOp) {}
    fn end_bin_expr(&mut self) {}

    fn begin_un_expr(&mut self, _expr: &UnExpr) -> Result<bool, E> {
        Ok(false)
    }
    fn unop(&mut self, _op: &UnOp) {}
    fn end_un_expr(&mut self) {}

    fn begin_suffixed_expr(&mut self, _expr: &SuffixedExpr) -> Result<bool, E> {
        Ok(false)
    }
    fn end_suffixed_expr(&mut self) {}

    fn name(&mut self, _name: &StringExpr) {}
    fn attr(&mut self, _attr: &StringExpr) {}
    fn method(&mut self, _method: &StringExpr) {}

    fn begin_index(&mut self, _expr: &Expr) -> Result<bool, E> {
        Ok(false)
    }
    fn end_index(&mut self) {}

    fn begin_func_args(&mut self, _args: &FuncArgs) -> Result<bool, E> {
        Ok(false)
    }
    fn end_func_args(&mut self) {}

    fn begin_paren_expr(&mut self, _expr: &Expr) -> Result<bool, E> {
        Ok(false)
    }
    fn end_paren_expr(&mut self) {}

    fn suffix(&mut self, _suf: &Suffix) -> Result<bool, E> {
        Ok(false)
    }

    fn error(&mut self, e: E, _source: &Source) -> Result<(), E> {
        Err(e)
    }

    fn comments(&mut self, _comments: &impl Comments) {}
}

pub mod ast_walker {
    use super::AstVisitor;
    use crate::{ast::*, types::Source};

    pub fn walk_block<T: AstVisitor<E>, E>(block: &Block, visitor: &mut T) -> Result<(), E> {
        for stat in block.stats.iter() {
            match walk_stat(stat, visitor) {
                Err(e) => {
                    return visitor.error(
                        e,
                        &Source {
                            line: 0,
                            col: 0,
                            length: 0,
                        },
                    )
                }
                _ => (),
            };
            visitor.stat_sep();
        }
        Ok(())
    }

    pub fn walk_stat<T: AstVisitor<E>, E>(stat: &Stat, visitor: &mut T) -> Result<(), E> {
        visitor.comments(stat);
        match stat {
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
        }
    }

    pub fn walk_ifstat<T: AstVisitor<E>, E>(stat: &IfStat, visitor: &mut T) -> Result<(), E> {
        let mut if_blocks = stat.cond_blocks.iter();
        if let Some(if_block) = if_blocks.next() {
            if !visitor.begin_if(&if_block.cond)? {
                walk_expr(&if_block.cond, visitor)?;
            }
            visitor.comments(&if_block.then);
            if !visitor.then(&if_block.block)? {
                walk_block(&if_block.block, visitor)?;
            }
            while let Some(else_if_block) = if_blocks.next() {
                if !visitor.begin_else_if(&else_if_block.cond)? {
                    walk_expr(&else_if_block.cond, visitor)?;
                }
                visitor.comments(&else_if_block.then);
                if !visitor.then(&else_if_block.block)? {
                    walk_block(&else_if_block.block, visitor)?;
                }
            }
            if let Some(else_block) = &stat.else_block {
                if !visitor.begin_else(else_block)? {
                    walk_block(else_block, visitor)?;
                }
            }
            visitor.comments(&stat.end);
            visitor.end_if();
        }
        Ok(())
    }

    pub fn walk_whilestat<T: AstVisitor<E>, E>(stat: &WhileStat, visitor: &mut T) -> Result<(), E> {
        if !visitor.begin_while(&stat.cond)? {
            walk_expr(&stat.cond, visitor)?;
        }
        visitor.comments(&stat.do_);
        if !visitor.begin_while_block(&stat.block)? {
            walk_block(&stat.block, visitor)?;
        }
        visitor.comments(&stat.end);
        visitor.end_while();
        Ok(())
    }

    pub fn walk_doblockstat<T: AstVisitor<E>, E>(stat: &DoBlock, visitor: &mut T) -> Result<(), E> {
        if !visitor.begin_do_block(&stat.block)? {
            walk_block(&stat.block, visitor)?;
        }
        visitor.comments(&stat.end);
        visitor.end_do_block();
        Ok(())
    }

    pub fn walk_forstat<T: AstVisitor<E>, E>(stat: &ForStat, visitor: &mut T) -> Result<(), E> {
        match stat {
            ForStat::ForNum(fornum) => walk_forenum(fornum, visitor),
            ForStat::ForList(forlist) => walk_forlist(forlist, visitor),
        }
    }

    pub fn walk_forenum<T: AstVisitor<E>, E>(stat: &ForNum, visitor: &mut T) -> Result<(), E> {
        if !visitor.begin_for_num(stat)? {
            visitor.name(&stat.var);
            visitor.comments(&stat.equal);
            visitor.for_enum_equal();
            walk_expr(&stat.init, visitor)?;
            visitor.comments(&stat.init_comma);
            visitor.expr_sep();
            walk_expr(&stat.limit, visitor)?;
            if let Some(expr) = &stat.step {
                visitor.comments(stat.limit_comma.as_ref().unwrap());
                visitor.expr_sep();
                walk_expr(expr, visitor)?;
            }
        }
        visitor.comments(&stat.do_);
        if !visitor.begin_for_block(&stat.body)? {
            walk_block(&stat.body, visitor)?;
        }
        visitor.comments(&stat.end);
        visitor.end_for();
        Ok(())
    }

    pub fn walk_forlist<T: AstVisitor<E>, E>(stat: &ForList, visitor: &mut T) -> Result<(), E> {
        if !visitor.begin_for_list(stat)? {
            stat.vars.vars.iter().enumerate().for_each(|(i, var)| {
                visitor.comments(var);
                visitor.name(var);
                if i != stat.vars.vars.len() - 1 {
                    visitor.comments(&stat.vars.delimiters[i]);
                    visitor.expr_sep();
                }
            });
            visitor.comments(&stat.in_);
            visitor.for_list_in();
            walk_exprlist(&stat.exprs, visitor)?;
        }
        visitor.comments(&stat.do_);
        if !visitor.begin_for_block(&stat.body)? {
            walk_block(&stat.body, visitor)?;
        }
        visitor.comments(&stat.end);
        visitor.end_for();
        Ok(())
    }

    pub fn walk_repeatstat<T: AstVisitor<E>, E>(
        stat: &RepeatStat,
        visitor: &mut T,
    ) -> Result<(), E> {
        if !visitor.begin_repeat(&stat.block)? {
            walk_block(&stat.block, visitor)?;
            visitor.comments(&stat.until);
            visitor.until();
            walk_expr(&stat.cond, visitor)?;
        }
        visitor.end_repeat();
        Ok(())
    }

    pub fn walk_funcstat<T: AstVisitor<E>, E>(stat: &FuncStat, visitor: &mut T) -> Result<(), E> {
        visitor.func(stat);
        walk_funcbody(&stat.body, visitor)
    }

    pub fn walk_localstat<T: AstVisitor<E>, E>(stat: &LocalStat, visitor: &mut T) -> Result<(), E> {
        visitor.local_stat(stat)
    }

    pub fn walk_labelstat<T: AstVisitor<E>, E>(stat: &LabelStat, visitor: &mut T) -> Result<(), E> {
        visitor.label_stat(stat)
    }

    pub fn walk_retstat<T: AstVisitor<E>, E>(stat: &RetStat, visitor: &mut T) -> Result<(), E> {
        visitor.ret_stat(stat)
    }

    pub fn walk_breakstat<T: AstVisitor<E>, E>(stat: &BreakStat, visitor: &mut T) -> Result<(), E> {
        visitor.break_stat(stat)
    }

    pub fn walk_gotostat<T: AstVisitor<E>, E>(stat: &GotoStat, visitor: &mut T) -> Result<(), E> {
        visitor.goto_stat(stat)
    }

    pub fn walk_assignstat<T: AstVisitor<E>, E>(
        stat: &AssignStat,
        visitor: &mut T,
    ) -> Result<(), E> {
        visitor.assign_stat(stat)
    }

    pub fn walk_callstat<T: AstVisitor<E>, E>(stat: &CallStat, visitor: &mut T) -> Result<(), E> {
        visitor.call_stat(stat)
    }

    pub fn walk_expr<T: AstVisitor<E>, E>(expr: &Expr, visitor: &mut T) -> Result<(), E> {
        visitor.comments(expr);
        if !visitor.expr(expr)? {
            match expr {
                Expr::Nil(_) => visitor.nil(),
                Expr::True(_) => visitor.true_(),
                Expr::False(_) => visitor.false_(),
                Expr::Float(f) => visitor.float(f),
                Expr::Int(i) => visitor.int(i),
                Expr::String(string) => visitor.string(string),
                Expr::VarArg(_) => visitor.vararg(),
                Expr::Name(s) => visitor.name(s),
                Expr::ParenExpr(expr) => walk_parenexpr(expr, visitor)?,
                Expr::FuncBody(body) => {
                    visitor.anonymous_func();
                    walk_funcbody(body, visitor)?
                }
                Expr::Table(t) => walk_table(t, visitor)?,
                Expr::BinExpr(expr) => walk_binexpr(expr, visitor)?,
                Expr::UnExpr(expr) => walk_unexpr(expr, visitor)?,
                Expr::SuffixedExpr(expr) => walk_suffixedexpr(expr, visitor)?,
            };
        }
        Ok(())
    }

    pub fn walk_funcbody<T: AstVisitor<E>, E>(body: &FuncBody, visitor: &mut T) -> Result<(), E> {
        if !visitor.begin_func_body(body)? {
            walk_block(&body.block, visitor)?;
        }
        visitor.comments(&body.end);
        visitor.end_func_body();
        Ok(())
    }

    pub fn walk_binexpr<T: AstVisitor<E>, E>(expr: &BinExpr, visitor: &mut T) -> Result<(), E> {
        if !visitor.begin_bin_expr(expr)? {
            walk_expr(&expr.left, visitor)?;
            visitor.binop(&expr.op);
            walk_expr(&expr.right, visitor)?;
        }
        visitor.end_bin_expr();
        Ok(())
    }

    pub fn walk_unexpr<T: AstVisitor<E>, E>(expr: &UnExpr, visitor: &mut T) -> Result<(), E> {
        if !visitor.begin_un_expr(expr)? {
            visitor.unop(&expr.op);
            walk_expr(&expr.expr, visitor)?;
        }
        visitor.end_un_expr();
        Ok(())
    }

    pub fn walk_suffixedexpr<T: AstVisitor<E>, E>(
        expr: &SuffixedExpr,
        visitor: &mut T,
    ) -> Result<(), E> {
        if !visitor.begin_suffixed_expr(expr)? {
            walk_expr(&expr.primary, visitor)?;
            for suf in expr.suffixes.iter() {
                if !visitor.suffix(suf)? {
                    match suf {
                        Suffix::Attr(_, attr) => visitor.attr(attr),
                        Suffix::Method(_, method) => visitor.method(method),
                        Suffix::Index(_, index, _) => walk_index(index, visitor)?,
                        Suffix::FuncArgs(args) => walk_funcargs(args, visitor)?,
                    }
                }
            }
        }
        visitor.end_suffixed_expr();
        Ok(())
    }

    pub fn walk_assinable<T: AstVisitor<E>, E>(
        assignable: &Assignable,
        visitor: &mut T,
    ) -> Result<(), E> {
        match assignable {
            Assignable::SuffixedExpr(s) => walk_suffixedexpr(s, visitor)?,
            Assignable::Name(s) => visitor.name(s),
        };
        Ok(())
    }

    pub fn walk_index<T: AstVisitor<E>, E>(expr: &Expr, visitor: &mut T) -> Result<(), E> {
        if !visitor.begin_index(expr)? {
            walk_expr(expr, visitor)?;
        }
        visitor.end_index();
        Ok(())
    }

    pub fn walk_funcargs<T: AstVisitor<E>, E>(args: &FuncArgs, visitor: &mut T) -> Result<(), E> {
        if !visitor.begin_func_args(args)? {
            match args {
                FuncArgs::String(s) => visitor.string(s),
                FuncArgs::Table(t) => walk_table(t, visitor)?,
                FuncArgs::Exprs(_, exprs, _) => walk_exprlist(exprs, visitor)?,
            }
        }
        visitor.end_func_args();
        Ok(())
    }

    pub fn walk_parenexpr<T: AstVisitor<E>, E>(expr: &Expr, visitor: &mut T) -> Result<(), E> {
        if !visitor.begin_paren_expr(expr)? {
            walk_expr(expr, visitor)?;
        }
        visitor.end_paren_expr();
        Ok(())
    }

    pub fn walk_table<T: AstVisitor<E>, E>(table: &Table, visitor: &mut T) -> Result<(), E> {
        if !visitor.begin_table(table)? {
            walk_fields(&table.fields, visitor)?;
        }
        visitor.end_table(table);
        Ok(())
    }

    pub fn walk_fields<T: AstVisitor<E>, E>(fields: &Vec<Field>, visitor: &mut T) -> Result<(), E> {
        for field in fields.iter() {
            walk_field(field, visitor)?;
            visitor.field_sep();
        }
        Ok(())
    }

    pub fn walk_field<T: AstVisitor<E>, E>(field: &Field, visitor: &mut T) -> Result<(), E> {
        match field {
            Field::RecField(field) => walk_recfield(field, visitor),
            Field::ListField(field) => walk_listfield(field, visitor),
        }
    }

    pub fn walk_recfield<T: AstVisitor<E>, E>(field: &RecField, visitor: &mut T) -> Result<(), E> {
        if !visitor.begin_rec_field(field)? {
            walk_fieldkey(&field.key, visitor)?;
            visitor.field_kv_sep();
            walk_expr(&field.value, visitor)?;
        }
        visitor.end_rec_field();
        Ok(())
    }

    pub fn walk_listfield<T: AstVisitor<E>, E>(
        field: &ListField,
        visitor: &mut T,
    ) -> Result<(), E> {
        walk_expr(&field.value, visitor)?;
        Ok(())
    }

    pub fn walk_fieldkey<T: AstVisitor<E>, E>(key: &FieldKey, visitor: &mut T) -> Result<(), E> {
        if !visitor.begin_field_key(key)? {
            match key {
                FieldKey::Name(s) => visitor.name(s),
                FieldKey::Expr(_, expr, _) => walk_expr(expr, visitor)?,
            };
        }
        visitor.end_field_key(key);
        Ok(())
    }

    pub fn walk_exprlist<T: AstVisitor<E>, E>(
        exprlist: &ExprList,
        visitor: &mut T,
    ) -> Result<(), E> {
        for (n, expr) in exprlist.exprs.iter().enumerate() {
            walk_expr(expr, visitor)?;
            if n < exprlist.exprs.len() - 1 {
                visitor.expr_sep();
            }
        }
        Ok(())
    }
}
