#[allow(unused_must_use)]
mod parser_tests {
    use rslua::ast::*;
    use rslua::lexer::Lexer;
    use rslua::parser::Parser;
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
        assert_eq!(
            try_parse(";;;;"),
            Block {
                stats: vec![]
            }
        );
    }

    #[test]
    fn ifstat() {
        let ast = try_parse(
            r#"
        if true then
        elseif true then 
        else end"#,
        );
        assert_eq!(
            ast,
            Block {
                stats: vec![Stat::IfStat(IfStat {
                    cond_blocks: vec![
                        CondBlock {
                            cond: Expr::True,
                            block: Block { stats: vec![] },
                        },
                        CondBlock {
                            cond: Expr::True,
                            block: Block { stats: vec![] },
                        },
                    ],
                    else_block: Block { stats: vec![] },
                })],
            }
        )
    }

    #[test]
    fn whilestat() {
        let ast = try_parse(r#"while true do end"#);
        assert_eq!(
            ast,
            Block {
                stats: vec![Stat::WhileStat(WhileStat {
                    cond: Expr::True,
                    block: Block { stats: vec![] },
                })],
            }
        )
    }

    #[test]
    fn forstat() {
        let forenum = try_parse("for i = 1, 10, 1 do end");
        let forlist = try_parse("for a, b in c, d do end");
        assert_eq!(
            forenum,
            Block {
                stats: vec![Stat::ForStat(ForStat::ForNum(ForNum {
                    var: String::from("i"),
                    init: Expr::Int(1),
                    limit: Expr::Int(10),
                    step: Some(Expr::Int(1)),
                    body: Block { stats: vec![] },
                }))],
            }
        );
        assert_eq!(
            forlist,
            Block {
                stats: vec![Stat::ForStat(ForStat::ForList(ForList {
                    vars: vec![String::from("a"), String::from("b"),],
                    exprs: vec![Expr::Name("c".to_string()), Expr::Name("d".to_string())],
                    body: Block { stats: vec![] },
                },),),],
            }
        )
    }

    #[test]
    fn doblock() {
        let ast = try_parse("do end");
        assert_eq!(
            ast,
            Block {
                stats: vec![Stat::DoBlock(DoBlock {
                    block: Block { stats: vec![] },
                },),],
            }
        )
    }

    #[test]
    fn repeatstat() {
        let ast = try_parse("repeat until a > 0");
        assert_eq!(
            ast,
            Block {
                stats: vec![Stat::RepeatStat(RepeatStat {
                    cond: Expr::BinExpr(BinExpr {
                        left: Box::new(Expr::Name("a".to_string())),
                        op: BinOp::Gt,
                        right: Box::new(Expr::Int(0))
                    }),
                    block: Block { stats: vec![] },
                })],
            }
        )
    }

    #[test]
    fn funcstat() {
        let ast = try_parse("function foo(a, b, c) end");
        assert_eq!(
            ast,
            Block {
                stats: vec![Stat::FuncStat(FuncStat {
                    func_type: FuncType::Global,
                    func_name: FuncName {
                        fields: vec!["foo".to_string()],
                        method: None,
                    },
                    body: FuncBody {
                        params: vec![
                            Param::Name(String::from("a")),
                            Param::Name(String::from("b")),
                            Param::Name(String::from("c"))
                        ],
                        block: Block { stats: vec![] },
                    },
                })]
            }
        )
    }

    #[test]
    fn localfunc() {
        let ast = try_parse("local function foo(a, b, c) end");
        assert_eq!(
            ast,
            Block {
                stats: vec![Stat::FuncStat(FuncStat {
                    func_type: FuncType::Local,
                    func_name: FuncName {
                        fields: vec!["foo".to_string()],
                        method: None,
                    },
                    body: FuncBody {
                        params: vec![
                            Param::Name(String::from("a")),
                            Param::Name(String::from("b")),
                            Param::Name(String::from("c"))
                        ],
                        block: Block { stats: vec![] },
                    },
                })]
            }
        )
    }

    #[test]
    fn localstat() {
        let ast = try_parse("local a, b, c = 1, 2, 3");
        assert_eq!(
            ast,
            Block {
                stats: vec![Stat::LocalStat(LocalStat {
                    names: vec!["a".to_string(), "b".to_string(), "c".to_string()],
                    exprs: vec![Expr::Int(1), Expr::Int(2), Expr::Int(3),],
                })],
            }
        )
    }

    #[test]
    fn labelstat() {
        let ast = try_parse("::LABEL::");
        assert_eq!(
            ast,
            Block {
                stats: vec![Stat::LabelStat(LabelStat {
                    label: "LABEL".to_string()
                })]
            }
        )
    }

    #[test]
    fn retstat() {
        let ast = try_parse("return 1 + a, b, c");
        assert_eq!(
            ast,
            Block {
                stats: vec![Stat::RetStat(RetStat {
                    exprs: vec![
                        Expr::BinExpr(BinExpr {
                            left: Box::new(Expr::Int(1)),
                            op: BinOp::Add,
                            right: Box::new(Expr::Name("a".to_string()))
                        }),
                        Expr::Name("b".to_string()),
                        Expr::Name("c".to_string()),
                    ],
                })],
            }
        )
    }

    #[test]
    fn gotostat() {
        let ast = try_parse("goto LABEL");
        assert_eq!(
            ast,
            Block {
                stats: vec![Stat::GotoStat(GotoStat {
                    label: "LABEL".to_string()
                })],
            }
        )
    }

    #[test]
    fn assignstat() {
        let ast = try_parse("a, b, c = 1, 2, 3");
        assert_eq!(
            ast,
            Block {
                stats: vec![Stat::AssignStat(AssignStat {
                    left: vec![
                        Assignable::Name("a".to_string()),
                        Assignable::Name("b".to_string()),
                        Assignable::Name("c".to_string()),
                    ],
                    right: vec![Expr::Int(1), Expr::Int(2), Expr::Int(3),],
                })],
            }
        )
    }

    #[test]
    fn callstat() {
        let ast = try_parse("foo(1, 2, 3)");
        assert_eq!(
            ast,
            Block {
                stats: vec![Stat::CallStat(CallStat {
                    call: Assignable::SuffixedExpr(SuffixedExpr {
                        primary: Box::new(Expr::Name("foo".to_string())),
                        suffixes: vec![Suffix::FuncArgs(FuncArgs::Exprs(vec![
                            Expr::Int(1),
                            Expr::Int(2),
                            Expr::Int(3),
                        ]))],
                    }),
                })],
            }
        )
    }

    #[test]
    fn exprlist() {
        let ast = try_parse("a(a, b, c)");
        assert_eq!(
            ast,
            Block {
                stats: vec![Stat::CallStat(CallStat {
                    call: Assignable::SuffixedExpr(SuffixedExpr {
                        primary: Box::new(Expr::Name("a".to_string())),
                        suffixes: vec![Suffix::FuncArgs(FuncArgs::Exprs(vec![
                            Expr::Name("a".to_string()),
                            Expr::Name("b".to_string()),
                            Expr::Name("c".to_string()),
                        ]))],
                    }),
                })],
            }
        )
    }

    #[test]
    fn table() {
        let ast1 = try_parse("local t = {1, 1.5, [[2]]}");
        let ast2 = try_parse("local t = {a = '1', ['b'] = 2, [a-1] = 3}");
        assert_eq!(
            ast1,
            Block {
                stats: vec![Stat::LocalStat(LocalStat {
                    names: vec!["t".to_string()],
                    exprs: vec![Expr::Table(Table {
                        fields: vec![
                            Field::ListField(Expr::Int(1)),
                            Field::ListField(Expr::Float(1.5)),
                            Field::ListField(Expr::String("2".to_string()))
                        ]
                    })]
                })],
            }
        );
        assert_eq!(
            ast2,
            Block {
                stats: vec![Stat::LocalStat(LocalStat {
                    names: vec!["t".to_string()],
                    exprs: vec![Expr::Table(Table {
                        fields: vec![
                            Field::RecFileld(RecField {
                                key: FieldKey::Name("a".to_string()),
                                value: Expr::String("1".to_string()),
                            }),
                            Field::RecFileld(RecField {
                                key: FieldKey::Expr(Expr::String("b".to_string())),
                                value: Expr::Int(2),
                            }),
                            Field::RecFileld(RecField {
                                key: FieldKey::Expr(Expr::BinExpr(BinExpr {
                                    op: BinOp::Minus,
                                    left: Box::new(Expr::Name("a".to_string())),
                                    right: Box::new(Expr::Int(1)),
                                })),
                                value: Expr::Int(3),
                            }),
                        ],
                    })],
                })],
            }
        );
    }

    #[test]
    fn callfunc() {
        let ast = try_parse("a:b {} 'literal' ()");
        assert_eq!(
            ast,
            Block {
                stats: vec![Stat::CallStat(CallStat {
                    call: Assignable::SuffixedExpr(SuffixedExpr {
                        primary: Box::new(Expr::Name("a".to_string())),
                        suffixes: vec![
                            Suffix::Method("b".to_string()),
                            Suffix::FuncArgs(FuncArgs::Table(Table { fields: vec![] })),
                            Suffix::FuncArgs(FuncArgs::String("literal".to_string())),
                            Suffix::FuncArgs(FuncArgs::Exprs(vec![])),
                        ],
                    }),
                })],
            },
        );
    }

    #[test]
    fn binop() {
        let ast1 = try_parse("return 1 .. 2 .. 3");
        let ast2 = try_parse("return 1 + 2 + 3");
        assert_eq!(
            ast1,
            Block {
                stats: vec![Stat::RetStat(RetStat {
                    exprs: vec![Expr::BinExpr(BinExpr {
                        op: BinOp::Concat,
                        left: Box::new(Expr::Int(1)),
                        right: Box::new(Expr::BinExpr(BinExpr {
                            op: BinOp::Concat,
                            left: Box::new(Expr::Int(2)),
                            right: Box::new(Expr::Int(3)),
                        })),
                    })],
                })],
            }
        );
        assert_eq!(
            ast2,
            Block {
                stats: vec![Stat::RetStat(RetStat {
                    exprs: vec![Expr::BinExpr(BinExpr {
                        op: BinOp::Add,
                        left: Box::new(Expr::BinExpr(BinExpr {
                            op: BinOp::Add,
                            left: Box::new(Expr::Int(1)),
                            right: Box::new(Expr::Int(2)),
                        })),
                        right: Box::new(Expr::Int(3)),
                    })],
                })],
            }
        );
    }

    #[test]
    fn method_call() {
        let ast = try_parse("str:sub(i,i)");
        assert_eq!(
            ast,
            Block {
                stats: vec![Stat::CallStat(CallStat {
                    call: Assignable::SuffixedExpr(SuffixedExpr {
                        primary: Box::new(Expr::Name("str".to_string())),
                        suffixes: vec![
                            Suffix::Method("sub".to_string()),
                            Suffix::FuncArgs(FuncArgs::Exprs(vec![
                                Expr::Name("i".to_string()),
                                Expr::Name("i".to_string()),
                            ])),
                        ]
                    }),
                })],
            }
        )
    }

    #[test]
    fn and() {
        let ast = try_parse("return a == 1 and b == 2");
        assert_eq!(
            ast,
            Block {
                stats: vec![Stat::RetStat(RetStat {
                    exprs: vec![Expr::BinExpr(BinExpr {
                        op: BinOp::And,
                        left: Box::new(Expr::BinExpr(BinExpr {
                            op: BinOp::Eq,
                            left: Box::new(Expr::Name("a".to_string())),
                            right: Box::new(Expr::Int(1)),
                        },)),
                        right: Box::new(Expr::BinExpr(BinExpr {
                            op: BinOp::Eq,
                            left: Box::new(Expr::Name("b".to_string())),
                            right: Box::new(Expr::Int(2)),
                        })),
                    })],
                })],
            }
        );
    }
}
