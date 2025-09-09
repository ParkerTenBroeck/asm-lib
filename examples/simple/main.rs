pub mod args;
pub mod indexed;
pub mod instructions;
pub mod label;
pub mod lang;
pub mod opcodes;
pub mod reg;
pub mod trans;

use std::path::Path;

use assembler::source::Sources;
pub use lang::MipsAssembler;
pub type NodeVal<'a> = assembler::expression::NodeVal<'a, MipsAssembler<'a>>;

pub fn main() {
    let src = r#"
.text

_start: .global; .type func;
 
        la sp, _stack_start
        jal main
    .loop:
        j .loop

main:
    la a0, message
    li a1, size(message)

    li v0, 1 // print syscall id
    syscall
    ret

    .size; .type func

.rodata
    message: .string "Hello, World!"; .size; .type obj; .global

.section .stack 
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
