use std::time::{Duration, SystemTime};

use crate::{
    LogCount, Logs, NodeOwned,
    simple::trans::{TranslationUnit, TranslationUnitMachine, merge::Merger},
};

pub struct LinkerResult<M: TranslationUnitMachine> {
    pub time: Duration,
    pub output: TranslationUnit<M>,
    pub log: Logs<NodeOwned>,
}

impl<M: TranslationUnitMachine> std::fmt::Display for LinkerResult<M> {
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
                "{BOLD}{RED}error{RESET}{BOLD}: could not link due to {errors} error(s). took {}s {RESET}",
                self.time.as_secs_f64(),
            )
        } else {
            writeln!(
                f,
                "{BOLD}{GREEN}Finished{RESET}{BOLD} in {}s {RESET}",
                self.time.as_secs_f64(),
            )
        }
    }
}

pub struct Linker<M: TranslationUnitMachine> {
    trans: TranslationUnit<M>,
    logs: Logs<NodeOwned>,
}

impl<M: TranslationUnitMachine> Linker<M> {
    pub fn new() -> Self {
        Self {
            trans: Default::default(),
            logs: Default::default(),
        }
    }

    pub fn link(mut self, parts: &[TranslationUnit<M>]) -> LinkerResult<M> {
        let start = SystemTime::now();
        let Some((first, remainder)) = parts.split_first() else {
            return LinkerResult {
                time: start.elapsed().unwrap(),
                output: Default::default(),
                log: Default::default(),
            };
        };
        self.trans = first.clone();

        {
            let mut merger = Merger::new();
            merger.merge_many(&mut self.trans, remainder);
            self.logs = merger.take_logs();
        }

        LinkerResult {
            time: start.elapsed().unwrap(),
            output: self.trans,
            log: self.logs,
        }
    }
}

impl<M: TranslationUnitMachine> Default for Linker<M> {
    fn default() -> Self {
        Self::new()
    }
}
