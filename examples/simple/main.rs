pub mod args;
pub mod indexed;
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

    _start: 
    add x1,x2,x3


    .section "stack"
    .align 1<<12
    .space 1<<12
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
