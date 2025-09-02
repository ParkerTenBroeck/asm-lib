pub mod assembler;
pub mod expression;
pub mod lex;
pub mod preprocess;
pub mod simple;

pub mod ansi;
pub mod config;
pub mod context;
pub mod logs;
pub mod node;
pub mod source;
pub mod util;

pub use bumpalo;

pub use assembler::lang::*;
pub use assembler::*;
pub use config::AssemblerConfig;
pub use context::*;
pub use lex::*;
pub use logs::*;
pub use preprocess::PreProcessor;

use bumpalo::Bump;
use std::path::Path;
use std::time::Instant;

use crate::source::Sources;

pub fn assemble<'a, L: AssemblyLanguage<'a>>(
    bump: &'a Bump,
    mut lang: L,
    mut preprocessor: PreProcessor<'a, L>,
    config: AssemblerConfig,
    source: &'a Path,
    supplier: Sources<'a>,
) -> AssemblerResult<L::AssembledResult> {
    let now = Instant::now();
    let mut context = Context::new(source, bump, config, supplier);

    let output = Assembler::assemble(&mut context, &mut lang, &mut preprocessor);

    let elapsed = now.elapsed().as_secs_f64();

    AssemblerResult {
        allocated: bump.allocated_bytes(),
        time: elapsed,
        output,
        log: context.take_logs(),
    }
}

pub struct AssemblerResult<T> {
    pub time: f64,
    pub allocated: usize,
    pub output: T,
    pub log: Logs<NodeOwned>,
}

impl<T> std::fmt::Display for AssemblerResult<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use crate::ansi::*;
        use crate::logs::*;
        let mut errors = 0;
        let mut warnings = 0;
        for log in &self.log {
            errors += log
                .parts
                .iter()
                .filter(|t| t.kind == LogKind::Error)
                .count();
            warnings += log
                .parts
                .iter()
                .filter(|t| t.kind == LogKind::Warning)
                .count();
            writeln!(f, "{log}")?;
        }

        if warnings > 0 {
            writeln!(
                f,
                "{BOLD}{YELLOW}warning{RESET}{BOLD}: {warnings} warning(s) emitted{RESET}"
            )?;
        }
        if errors > 0 {
            writeln!(
                f,
                "{BOLD}{RED}error{RESET}{BOLD}: could not assemble due to {errors} error(s). took {}s allocated {}b{RESET}",
                self.time, self.allocated
            )
        } else {
            writeln!(
                f,
                "{BOLD}{GREEN}Finished{RESET}{BOLD} in {}s allocated {}b{RESET}",
                self.time, self.allocated
            )
        }
    }
}
