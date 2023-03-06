#[allow(unused_must_use)]
mod parser_tests {
    use rslua::ast::*;
    use rslua::tokens::*;
    use rslua::lexer::Lexer;
    use rslua::parser::Parser;
    use rslua::types::Source;
    use std::fs::File;
    use std::io::prelude::*;

    fn try_parse(input: &str) -> Block {
        let mut lexer = Lexer::new();
        lexer.set_debug(true);
        if let Ok(tokens) = lexer.run(input) {
            let mut parser = Parser::new();
            parser.set_debug(true);
            if let Ok(ast) = parser.run(tokens) {
                println!("{:#?}", ast);
                return ast;
            }
        }
        unreachable!()
    }

    #[test]
    fn parser_practical() -> std::io::Result<()> {
        let mut file = File::open(r"lua/json.lua")?;
        let mut content = String::new();
        file.read_to_string(&mut content)?;
        try_parse(&content);
        Ok(())
    }

    #[test]
    fn empty_stat() {
        assert_eq!(try_parse(";;;;"), Block { stats: vec![] });
    }

    #[test]
    fn ifstat() {
        let ast = try_parse(
            r#"
        if true then
        elseif true then 
        else end"#,
        );
        let stat = &ast.stats[0];
        match stat {
            Stat::IfStat(if_) => {
                assert_eq!(if_.cond_blocks.len(), 2);
                assert!(matches!(if_.else_, Some(..)));
                assert!(matches!(if_.cond_blocks[0].cond, Expr::True(..)));
                assert!(matches!(if_.cond_blocks[1].cond, Expr::True(..)));
            },
            _ => unreachable!()
        }
    }

    #[test]
    fn whilestat() {
        let ast = try_parse(r#"while true do end"#);
        let stat = &ast.stats[0];
        match stat {
            Stat::WhileStat(while_) => {
                assert!(matches!(while_.cond, Expr::True(..)));
            },
            _ => unreachable!()
        }
    }

    #[test]
    fn forstat() {
        let fornum = try_parse("for i = 1, 10, 1 do end");
        let fornum_stat = &fornum.stats[0];
        match fornum_stat {
            Stat::ForStat(for_) => {
                match &for_ {
                    ForStat::ForNum(for_num) => {
                        assert_eq!(for_num.var.value(), "i");
                        assert_eq!(for_num.init.unwrap_as_int(), 1);
                        assert_eq!(for_num.limit.unwrap_as_int(), 10);
                        assert_eq!(for_num.step.as_ref().unwrap().unwrap_as_int(), 1);
                    },
                    _ => unreachable!()
                }
            },
            _ => unreachable!()
        }

        let forlist = try_parse("for a, b in c, d do end");
        let forlist_stat = &forlist.stats[0];
        match forlist_stat {
            Stat::ForStat(for_) => {
                match &for_ {
                    ForStat::ForList(for_list) => {
                        assert_eq!(for_list.vars.vars[0].value(), "a");
                        assert_eq!(for_list.vars.vars[1].value(), "b");
                        assert_eq!(for_list.exprs.exprs[0].unwrap_as_name().value(), "c");
                        assert_eq!(for_list.exprs.exprs[1].unwrap_as_name().value(), "d");
                    },
                    _ => unreachable!()
                }
            },
            _ => unreachable!()
        }
    }

    #[test]
    fn doblock() {
        let ast = try_parse("do end");
        let stat = &ast.stats[0];
        assert!(matches!(stat, Stat::DoBlock(..)));
    }

    #[test]
    fn repeatstat() {
        let ast = try_parse("repeat until a > 0");
        match &ast.stats[0] {
            Stat::RepeatStat(repeat) => {
                match &repeat.cond {
                    Expr::BinExpr(expr) => {
                        assert_eq!(expr.left.unwrap_as_name().value(), "a");
                        assert!(matches!(expr.op, BinOp::Gt(..)));
                        assert_eq!(expr.right.unwrap_as_int(), 0);
                    },
                    _ => unreachable!(),
                }
            },
            _ => unreachable!(),
        };
    }

    #[test]
    fn funcstat() {
        let ast = try_parse("function foo(a, b, c) end");
        match &ast.stats[0] {
            Stat::FuncStat(stat) => {
                assert_eq!(stat.func_type, FuncType::Global);
                assert_eq!(stat.func_name.fields.vars.len(), 1);
                assert_eq!(stat.func_name.fields.vars[0].value(), "foo");
                assert_eq!(stat.body.params.params.len(), 3);
                assert_eq!(stat.body.params.params[0].unwrap_as_name(), "a");
                assert_eq!(stat.body.params.params[1].unwrap_as_name(), "b");
                assert_eq!(stat.body.params.params[2].unwrap_as_name(), "c");
            },
            _ => unreachable!()
        };
    }

    #[test]
    fn localfunc() {
        let ast = try_parse("local function foo(a, b, c) end");
        match &ast.stats[0] {
            Stat::FuncStat(stat) => {
                assert!(matches!(stat.func_type, FuncType::Local(..)));
                assert_eq!(stat.func_name.fields.vars.len(), 1);
                assert_eq!(stat.func_name.fields.vars[0].value(), "foo");
                assert_eq!(stat.body.params.params.len(), 3);
                assert_eq!(stat.body.params.params[0].unwrap_as_name(), "a");
                assert_eq!(stat.body.params.params[1].unwrap_as_name(), "b");
                assert_eq!(stat.body.params.params[2].unwrap_as_name(), "c");
            },
            _ => unreachable!()
        };
    }

    #[test]
    fn localstat() {
        let ast = try_parse("local a, b, c = 1, 2, 3");
        match &ast.stats[0] {
            Stat::LocalStat(stat) => {
                assert_eq!(stat.names.vars[0].value(), "a");
                assert_eq!(stat.names.vars[1].value(), "b");
                assert_eq!(stat.names.vars[2].value(), "c");
                assert_eq!(stat.exprs.as_ref().unwrap().exprs[0].unwrap_as_int(), 1);
                assert_eq!(stat.exprs.as_ref().unwrap().exprs[1].unwrap_as_int(), 2);
                assert_eq!(stat.exprs.as_ref().unwrap().exprs[2].unwrap_as_int(), 3);
            },
            _ => unreachable!()
        };
    }

    #[test]
    fn labelstat() {
        let ast = try_parse("::LABEL::");
        match &ast.stats[0] {
            Stat::LabelStat(label) => assert_eq!(label.label.value(), "LABEL"),
            _ => unreachable!()
        };
    }

    #[test]
    fn retstat() {
        let ast = try_parse("return 1 + a, b, c");
        match &ast.stats[0] {
            Stat::RetStat(ret) => {
                let expr = &ret.exprs.as_ref().unwrap().exprs[0];
                match expr {
                    Expr::BinExpr(bin) => {
                        assert!(matches!(bin.op, BinOp::Add(..)));
                        assert_eq!(bin.left.unwrap_as_int(), 1);
                        assert_eq!(bin.right.unwrap_as_name().value(), "a");
                    },
                    _ => unreachable!()
                }
                assert_eq!(ret.exprs.as_ref().unwrap().exprs[1].unwrap_as_name().value(), "b");
                assert_eq!(ret.exprs.as_ref().unwrap().exprs[2].unwrap_as_name().value(), "c");
            },
            _ => unreachable!()
        };
    }

    #[test]
    fn gotostat() {
        let ast = try_parse("goto LABEL");
        match &ast.stats[0] {
            Stat::GotoStat(goto) => {
                assert_eq!(goto.label.value(), "LABEL");
            }
            _ => unreachable!()
        }
    }

    #[test]
    fn assignstat() {
        let ast = try_parse("a, b, c = 1, 2, 3");
        match &ast.stats[0] {
            Stat::AssignStat(stat) => {
                match &stat.left.assignables[0] {
                    Assignable::Name(name) => assert_eq!(name.value(), "a"),
                    _ => unreachable!()
                };
                match &stat.left.assignables[1] {
                    Assignable::Name(name) => assert_eq!(name.value(), "b"),
                    _ => unreachable!()
                };
                match &stat.left.assignables[2] {
                    Assignable::Name(name) => assert_eq!(name.value(), "c"),
                    _ => unreachable!()
                };

                assert_eq!(stat.right.exprs[0].unwrap_as_int(), 1);
                assert_eq!(stat.right.exprs[1].unwrap_as_int(), 2);
                assert_eq!(stat.right.exprs[2].unwrap_as_int(), 3);
                    
            },
            _ => unreachable!()
        }
    }

    #[test]
    fn callstat() {
        let ast = try_parse("foo(1, 2, 3)");
        match &ast.stats[0] {
            Stat::CallStat(call) => {
                match &call.call {
                    Assignable::SuffixedExpr(expr) => {
                        assert_eq!(expr.primary.unwrap_as_name().value(), "foo");
                        let func_args = expr.suffixes[0].unwrap_as_func_args();
                        match func_args {
                            FuncArgs::Exprs(_, args, _) => {
                                assert_eq!(args.exprs[0].unwrap_as_int(), 1);
                                assert_eq!(args.exprs[1].unwrap_as_int(), 2);
                                assert_eq!(args.exprs[2].unwrap_as_int(), 3);
                            },
                            _ => unreachable!()
                        }
                    },
                    _ => unreachable!()
                }
            }
            _ => unreachable!()
        }
    }

    #[test]
    fn exprlist() {
        let ast = try_parse("a(a, b, c)");
        match &ast.stats[0] {
            Stat::CallStat(call) => {
                match &call.call {
                    Assignable::SuffixedExpr(expr) => {
                        assert_eq!(expr.primary.unwrap_as_name().value(), "a");
                        let func_args = expr.suffixes[0].unwrap_as_func_args();
                        match func_args {
                            FuncArgs::Exprs(_, args, _) => {
                                assert_eq!(args.exprs[0].unwrap_as_name().value(), "a");
                                assert_eq!(args.exprs[1].unwrap_as_name().value(), "b");
                                assert_eq!(args.exprs[2].unwrap_as_name().value(), "c");
                            },
                            _ => unreachable!()
                        }
                    }
                    _ => unreachable!()
                }
            },
            _ => unreachable!()
        }
    }

    #[test]
    fn table() {
        let ast1 = try_parse("local t = {1, 1.5, [[2]]}");
        let ast2 = try_parse("local t = {a = '1', ['b'] = 2, [a-1] = 3}");
        match &ast1.stats[0] {
            Stat::LocalStat(local) => {
                assert_eq!(local.names.vars[0].value(), "t");
                match &local.exprs.as_ref().unwrap().exprs[0] {
                    Expr::Table(table) => {
                        let field = &table.fields;
                        assert_eq!(field[0].unwrap_as_list_field().value.unwrap_as_int(), 1);
                        assert_eq!(field[1].unwrap_as_list_field().value.unwrap_as_float().to_ne_bytes(), 1.5_f64.to_ne_bytes());
                        assert_eq!(field[2].unwrap_as_list_field().value.unwrap_as_string(), "2");
                    }
                    _ => unreachable!()
                }
            },
            _ => unreachable!()
        }

        match &ast2.stats[0] {
            Stat::LocalStat(local) => {
                assert_eq!(local.names.vars[0].value(), "t");
                match &local.exprs.as_ref().unwrap().exprs[0] {
                    Expr::Table(table) => {
                        let field = &table.fields;
                        assert_eq!(field[0].unwrap_as_rec_field().key.unwrap_as_name().value(), "a");
                        assert_eq!(field[0].unwrap_as_rec_field().value.unwrap_as_string(), "1");
                        assert_eq!(field[1].unwrap_as_rec_field().key.unwrap_as_expr().unwrap_as_string(), "b");
                        assert_eq!(field[1].unwrap_as_rec_field().value.unwrap_as_int(), 2);
                        let expr = field[2].unwrap_as_rec_field().key.unwrap_as_expr();
                        match &expr {
                            Expr::BinExpr(bin) => {
                                assert!(matches!(bin.op, BinOp::Minus(..)));
                                assert_eq!(bin.left.clone().to_assignable().unwrap_as_name().value(), "a");
                                assert_eq!(bin.right.unwrap_as_int(), 1);
                            },
                            _ => unreachable!()
                        }
                        assert_eq!(field[2].unwrap_as_rec_field().value.unwrap_as_int(), 3);
                    }
                    _ => unreachable!()
                }
            },
            _ => unreachable!()
        }
    }

    // #[test]
    // fn callfunc() {
    //     let ast = try_parse("a:b {} 'literal' ()");
    //     assert_eq!(
    //         ast,
    //         Block {
    //             stats: vec![Stat::CallStat(CallStat {
    //                 call: Assignable::SuffixedExpr(SuffixedExpr {
    //                     primary: Box::new(Expr::Name("a".to_string())),
    //                     suffixes: vec![
    //                         Suffix::Method("b".to_string()),
    //                         Suffix::FuncArgs(FuncArgs::Table(Table { fields: vec![] })),
    //                         Suffix::FuncArgs(FuncArgs::String("literal".to_string())),
    //                         Suffix::FuncArgs(FuncArgs::Exprs(vec![])),
    //                     ],
    //                 }),
    //             })
    //             .to_stat_info()],
    //         },
    //     );
    // }

    // #[test]
    // fn binop() {
    //     let ast1 = try_parse("return 1 .. 2 .. 3");
    //     let ast2 = try_parse("return 1 + 2 + 3");
    //     assert_eq!(
    //         ast1,
    //         Block {
    //             stats: vec![Stat::RetStat(RetStat {
    //                 exprs: vec![Expr::BinExpr(BinExpr {
    //                     op: BinOp::Concat,
    //                     left: Box::new(Expr::Int(1)),
    //                     right: Box::new(Expr::BinExpr(BinExpr {
    //                         op: BinOp::Concat,
    //                         left: Box::new(Expr::Int(2)),
    //                         right: Box::new(Expr::Int(3)),
    //                     })),
    //                 })],
    //             })
    //             .to_stat_info()],
    //         }
    //     );
    //     assert_eq!(
    //         ast2,
    //         Block {
    //             stats: vec![Stat::RetStat(RetStat {
    //                 exprs: vec![Expr::BinExpr(BinExpr {
    //                     op: BinOp::Add,
    //                     left: Box::new(Expr::BinExpr(BinExpr {
    //                         op: BinOp::Add,
    //                         left: Box::new(Expr::Int(1)),
    //                         right: Box::new(Expr::Int(2)),
    //                     })),
    //                     right: Box::new(Expr::Int(3)),
    //                 })],
    //             })
    //             .to_stat_info()],
    //         }
    //     );
    // }

    // #[test]
    // fn method_call() {
    //     let ast = try_parse("str:sub(i,i)");
    //     assert_eq!(
    //         ast,
    //         Block {
    //             stats: vec![Stat::CallStat(CallStat {
    //                 call: Assignable::SuffixedExpr(SuffixedExpr {
    //                     primary: Box::new(Expr::Name("str".to_string())),
    //                     suffixes: vec![
    //                         Suffix::Method("sub".to_string()),
    //                         Suffix::FuncArgs(FuncArgs::Exprs(vec![
    //                             Expr::Name("i".to_string()),
    //                             Expr::Name("i".to_string()),
    //                         ])),
    //                     ]
    //                 }),
    //             })
    //             .to_stat_info()],
    //         }
    //     )
    // }

    // #[test]
    // fn and() {
    //     let ast = try_parse("return a == 1 and b == 2");
    //     assert_eq!(
    //         ast,
    //         Block {
    //             stats: vec![Stat::RetStat(RetStat {
    //                 exprs: vec![Expr::BinExpr(BinExpr {
    //                     op: BinOp::And,
    //                     left: Box::new(Expr::BinExpr(BinExpr {
    //                         op: BinOp::Eq,
    //                         left: Box::new(Expr::Name("a".to_string())),
    //                         right: Box::new(Expr::Int(1)),
    //                     },)),
    //                     right: Box::new(Expr::BinExpr(BinExpr {
    //                         op: BinOp::Eq,
    //                         left: Box::new(Expr::Name("b".to_string())),
    //                         right: Box::new(Expr::Int(2)),
    //                     })),
    //                 })],
    //             })
    //             .to_stat_info()],
    //         }
    //     );
    // }
}
