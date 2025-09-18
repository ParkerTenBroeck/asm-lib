pub mod args;
pub mod eval;
pub mod indexed;
pub mod instructions;
pub mod label;
pub mod lang;
pub mod opcodes;
pub mod reg;
pub mod trans;

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use assembler::{simple::trans::link::Linker, source::Sources};
pub use lang::MipsAssembler;
pub type NodeVal<'a> = assembler::expression::NodeVal<'a, MipsAssembler<'a>>;

fn example1() {
    let src = r#"
.section .text.start

_start: .global; .type func;
 
        la sp, _stack_start
        jal main
    .loop:
        j .loop


.text

main: .global; .type func;
    la a0, message
    li a1, size(message)+3

    j pcrel(meow)

    li v0, 1 // print syscall id
    // actually do the call
    syscall
    ret

    .size;


random_function: .global; .type func;

    la a0, .text
    beq a0, zero, 1f
    ret
    1:
    b 1b

.zero_reg:
    lw x3, zero
    lw x3, zero+23
    lw x3, zero[23]
    lw x3, 23[zero]
    lw x3, 0xFFFF[zero]
    lw x3, label
    lw x3, label+44
.other_reg:
    lw x3, a0
    lw x3, a0+23
    lw x3, a0[23]
    lw x3, 23[a0]
    lw x3, 0xFFFF[a0]
    lw x3, label[a0]
    lw x3, 44+label[a0]
.asm_reserved:
    lw x3, at
    lw x3, at+23
    lw x3, at[23]
    lw x3, 23[at]
    lw x3, 0xFFFF[at]
    lw x3, label[at]
    lw x3, 44+label[at]

.size;


.rodata
    message: .string "Hello, World!"; .size; .type obj; .global

    main_fn_ptr: .values main; .size; .type obj; .global

.section .data.stack 
    _stack_end: .global; .align 1<<12; .space 1<<12
    _stack_start: .size 1<<12; .global
    "#;
    let res = assembler::assemble(
        &Default::default(),
        MipsAssembler::new(),
        Default::default(),
        Default::default(),
        "file.asm".as_ref(),
        Sources::new(Box::new(|path, _| {
            if path == Path::new("file.asm") {
                Ok(assembler::source::SourceContents::Text(src))
            } else {
                Err("file does not exist".into())
            }
        })),
    );

    println!("{res}");
    println!("{}", res.output);
}

fn example2() {
    let mut map = HashMap::new();
    map.insert(
        PathBuf::from("main.asm"),
        r#"
    
.section .text.start

_start: .global; .type func;
 
        la sp, _stack_start
        jal main
    .loop:
        j .loop


.text

main: .global; .type func;
    jal other_function
    jal some_function
    la a0, message
    li a1, size(message)+3

    j pcrel(meow)

    li v0, 1 // print syscall id
    // actually do the call
    syscall
    ret

.size


.data

other_message: .global;
    .cstring c"MESSAGE!"
    
    "#,
    );
    map.insert(
        PathBuf::from("other.asm"),
        r#"


    .section .text.meow

other_function: .global; .type func;
    ret
.size

.data

other_data:  .global;
    .values some_function 

.section .data.stack 
    _stack_end: .global; .align 1<<12; .space 1<<12
    _stack_start: .size 1<<12; .global
    "#,
    );
    map.insert(
        PathBuf::from("something.asm"),
        r#"
    
.text

some_function: .global; .type func;
    // idk do something?
    jal private_function
    ret

.size

other_data: .global;

private_function: .type func;
    // idk do something?
    ret
.size

private_function:
    
.data

message:.global;
.string "Hello, World"
    
    "#,
    );

    let units: Vec<_> = map
        .iter()
        .filter(|(path, _)| path.extension() == Some("asm".as_ref()))
        .map(|(path, _)| {
            let result = assembler::assemble(
                &Default::default(),
                MipsAssembler::new(),
                Default::default(),
                Default::default(),
                path.as_ref(),
                Sources::new(Box::new(|path, _| {
                    if let Some(src) = map.get(path) {
                        Ok(assembler::source::SourceContents::Text(src))
                    } else {
                        Err("file does not exist".into())
                    }
                })),
            );
            println!("{result}");
            result.output
        })
        .collect();

    let result = Linker::new().link(units.as_slice());

    println!("{result}");
    println!("{}", result.output);
}

pub fn main() {
    example2()
}
