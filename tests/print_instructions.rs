use rslua::opcodes::*;
use std::io::Write;
#[test]
fn print_instruction() {
    let mut s = Vec::<u8>::new();
    // IA
    writeln!(
        &mut s,
        "{:?}",
        Instruction::create_ABC(OpCode::LoadKx, 123, 0, 0)
    )
    .unwrap();
    // IAB
    writeln!(
        &mut s,
        "{:?}",
        Instruction::create_ABC(OpCode::Move, 123, 456, 0)
    )
    .unwrap();
    // IABx
    writeln!(
        &mut s,
        "{:?}",
        Instruction::create_ABx(OpCode::LoadK, 123, 456)
    )
    .unwrap();
    // IABC
    writeln!(
        &mut s,
        "{:?}",
        Instruction::create_ABC(OpCode::LoadBool, 123, 456, 511)
    )
    .unwrap();
    // IAsBx
    writeln!(
        &mut s,
        "{:?}",
        Instruction::create_AsBx(OpCode::Jmp, 123, -255)
    )
    .unwrap();
    // IAC
    writeln!(
        &mut s,
        "{:?}",
        Instruction::create_ABC(OpCode::TForCall, 123, 456, 511)
    )
    .unwrap();
    let string = String::from_utf8(s).ok().unwrap();
    println!("{}", string);
    assert_eq!(
        string,
        r#"Instruction [LoadKx    123            ]
Instruction [Move      123  456       ]
Instruction [LoadK     123  456       ]
Instruction [LoadBool  123  456  511  ]
Instruction [Jmp       123  -255      ]
Instruction [TForCall  123       511  ]
"#
    )
}
