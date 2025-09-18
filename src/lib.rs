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
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

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

    AssemblerResult {
        path: source.to_path_buf(),
        allocated: bump.allocated_bytes(),
        time: now.elapsed(),
        output,
        log: context.take_logs(),
    }
}

pub struct AssemblerResult<T> {
    pub path: PathBuf,
    pub time: Duration,
    pub allocated: usize,
    pub output: T,
    pub log: Logs<NodeOwned>,
}

impl<T> std::fmt::Display for AssemblerResult<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use crate::ansi::*;
        let LogCount {
            errors, warnings, ..
        } = self.log.display(f)?;

        if warnings > 0 {
            writeln!(
                f,
                "{BOLD}{YELLOW}warning{RESET}{BOLD}: {warnings} warning(s) emitted{RESET}"
            )?;
        }
        if errors > 0 {
            writeln!(
                f,
                "{BOLD}{RED}error{RESET}{BOLD}: could not assemble '{}' due to {errors} error(s). took {}s allocated {}b{RESET}",
                self.path.display(),
                self.time.as_secs_f64(),
                self.allocated
            )
        } else {
            writeln!(
                f,
                "{BOLD}{GREEN}Finished{RESET}{BOLD} assembling '{}' in {}s allocated {}b{RESET}",
                self.path.display(),
                self.time.as_secs_f64(),
                self.allocated
            )
        }
    }
}
