use crate::ansi::*;
use crate::node::NodeTrait;
use crate::node::Source;
use std::fmt::Write;

#[derive(Debug, Clone)]
pub struct Logs<T: NodeTrait> {
    logs: Vec<LogEntry<T>>,
}

impl<T: NodeTrait> Default for Logs<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct LogCount {
    pub total: usize,
    pub warnings: usize,
    pub errors: usize,
}

impl<T: NodeTrait> Logs<T> {
    pub const fn new() -> Self {
        Self { logs: Vec::new() }
    }

    pub fn report(&mut self, entry: LogEntry<T>) {
        self.logs.push(entry);
    }

    pub fn report_error_locless(&mut self, msg: impl ToString) {
        self.report(LogEntry::new().error_locless(msg));
    }

    pub fn report_warning_locless(&mut self, msg: impl ToString) {
        self.report(LogEntry::new().warning_locless(msg));
    }

    pub fn report_info_locless(&mut self, msg: impl ToString) {
        self.report(LogEntry::new().info_locless(msg));
    }

    pub fn report_hint_locless(&mut self, msg: impl ToString) {
        self.report(LogEntry::new().hint_locless(msg));
    }

    pub fn report_error(&mut self, node: T, error: impl ToString) {
        self.report(LogEntry::new().error(node, error));
    }

    pub fn report_warning(&mut self, node: T, error: impl ToString) {
        self.report(LogEntry::new().warning(node, error));
    }

    pub fn report_info(&mut self, node: T, error: impl ToString) {
        self.report(LogEntry::new().info(node, error));
    }

    pub fn take(&mut self) -> Self {
        let mut other = Self::new();
        std::mem::swap(self, &mut other);
        other
    }

    pub fn display(&self, f: &mut std::fmt::Formatter) -> Result<LogCount, std::fmt::Error> {
        let mut errors = 0;
        let mut warnings = 0;
        for log in &self.logs {
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
        Ok(LogCount {
            total: self.logs.len(),
            warnings,
            errors,
        })
    }
}

impl<'a, T: NodeTrait> IntoIterator for &'a Logs<T> {
    type Item = &'a LogEntry<T>;
    type IntoIter = std::slice::Iter<'a, LogEntry<T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.logs.iter()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogKind {
    Error,
    Warning,
    Info,
    Hint,
    From,
    Pasted,
    Generated,
    Included,
}

#[derive(Debug, Clone)]
pub struct LogPart<T: NodeTrait> {
    pub node: Option<T>,
    pub kind: LogKind,
    pub msg: Option<String>,
}

#[derive(Default, Debug, Clone)]
pub struct LogEntry<T: NodeTrait> {
    pub parts: Vec<LogPart<T>>,
}

impl<T: NodeTrait + Clone> LogEntry<T> {
    pub fn new() -> Self {
        Self { parts: Vec::new() }
    }

    pub fn error(self, node_id: T, msg: impl ToString) -> Self {
        self.add(node_id, LogKind::Error, msg)
    }

    pub fn warning(self, node_id: T, msg: impl ToString) -> Self {
        self.add(node_id, LogKind::Warning, msg)
    }

    pub fn info(self, node_id: T, msg: impl ToString) -> Self {
        self.add(node_id, LogKind::Info, msg)
    }

    pub fn hint(self, node_id: T, msg: impl ToString) -> Self {
        self.add(node_id, LogKind::Hint, msg)
    }

    pub fn add(mut self, node_id: T, kind: LogKind, msg: impl ToString) -> Self {
        let mut node_id = Some(&node_id);
        let mut msg = Some(msg.to_string());
        let mut kind = kind;

        let position = self.parts.len();
        while let Some(node) = node_id {
            self.parts.insert(
                position,
                LogPart {
                    node: node_id.map(|f| (*f).clone()),
                    kind,
                    msg: msg.take(),
                },
            );
            if position != 0 {
                break;
            }
            use crate::context::Parent;
            match node.parent() {
                Parent::None => kind = LogKind::From,
                Parent::Included { .. } => kind = LogKind::Included,
                Parent::Pasted { .. } => kind = LogKind::Pasted,
                Parent::Generated { .. } => kind = LogKind::Generated,
            }

            node_id = node.parent().parent();
        }

        self
    }

    pub fn error_locless(self, msg: impl ToString) -> Self {
        self.add_locless(LogKind::Error, msg)
    }

    pub fn warning_locless(self, msg: impl ToString) -> Self {
        self.add_locless(LogKind::Warning, msg)
    }

    pub fn info_locless(self, msg: impl ToString) -> Self {
        self.add_locless(LogKind::Info, msg)
    }

    pub fn hint_locless(self, msg: impl ToString) -> Self {
        self.add_locless(LogKind::Hint, msg)
    }

    pub fn add_locless(mut self, kind: LogKind, msg: impl ToString) -> LogEntry<T> {
        self.parts.push(LogPart {
            node: None,
            kind,
            msg: Some(msg.to_string()),
        });
        self
    }
}

pub fn expand_range_to_start_end_line(
    range: std::ops::Range<usize>,
    source: &str,
) -> (std::ops::Range<usize>, &str) {
    let start = source[..range.start]
        .char_indices()
        .rev()
        .find_map(|c| (c.1 == '\n').then_some(c.0.saturating_add(1)))
        .unwrap_or(0);
    let to_end = if range.end < source.len() {
        &source[range.end..]
    } else {
        Default::default()
    };
    let end = range.end
        + to_end
            .chars()
            .position(|c| c == '\n')
            .unwrap_or(to_end.len());
    let expanded_range = start..end.min(source.len());

    (expanded_range.clone(), &source[expanded_range])
}

impl<T: NodeTrait> std::fmt::Display for LogEntry<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for part in self.parts.iter() {
            match part.kind {
                LogKind::Error => write!(f, "{BOLD}{RED}error{RESET}{RESET}{BOLD}")?,
                LogKind::Warning => write!(f, "{BOLD}{YELLOW}warning{RESET}{RESET}{BOLD}")?,
                LogKind::Info => write!(f, "{BOLD}{BLUE}info{RESET}{RESET}{BOLD}")?,
                LogKind::From => write!(f, "{BOLD}{GREEN}from{RESET}{RESET}{BOLD}")?,
                LogKind::Hint => write!(f, "{BOLD}{GREEN}hint{RESET}{RESET}{BOLD}")?,
                LogKind::Pasted => write!(f, "{BOLD}{GREEN}pasted by{RESET}{RESET}{BOLD}")?,
                LogKind::Generated => write!(f, "{BOLD}{GREEN}generated by{RESET}{RESET}{BOLD}")?,
                LogKind::Included => write!(f, "{BOLD}{GREEN}included by{RESET}{RESET}{BOLD}")?,
            }

            if let Some(msg) = &part.msg {
                write!(f, ": {msg}")?;
            }

            let Some(node) = part.node.as_ref() else {
                writeln!(f)?;
                continue;
            };
            let span = node.span();
            let source = &node.source();

            let error_range = span.offset as usize..(span.offset as usize + span.len as usize);
            let (expanded_range, expanded) =
                expand_range_to_start_end_line(error_range.clone(), source.contents());
            let mut line = span.line as usize + 1;
            let end_line = line + expanded.lines().count();
            let max_line_digits = end_line.ilog10() as usize + 1;

            writeln!(
                f,
                "{BLUE}{BOLD}\n{: >max_line_digits$}---> {RESET}{}:{}:{}",
                " ",
                source.path().display(),
                line,
                span.col + 1
            )?;
            writeln!(f, "{BLUE}{BOLD}{: >max_line_digits$} |", "")?;

            let mut index = expanded_range.start;
            for line_contents in expanded.split('\n') {
                write!(f, "{line: >max_line_digits$} |{RESET} ")?;
                for char in line_contents.chars() {
                    if char == '\t' {
                        f.write_char(' ')?
                    } else {
                        f.write_char(char)?
                    }
                }
                writeln!(f)?;
                write!(f, "{BLUE}{BOLD}{: >max_line_digits$} | ", "")?;
                for c in line_contents.chars() {
                    if error_range.contains(&index) {
                        write!(f, "~")?;
                    } else {
                        write!(f, " ")?;
                    }
                    index += c.len_utf8();
                }
                //nl
                if error_range.contains(&index) || error_range.is_empty() {
                    write!(f, "~")?;
                } else {
                    write!(f, " ")?;
                }
                index += '\n'.len_utf8();
                writeln!(f)?;

                line += 1;
            }
            write!(f, "{RESET}")?
        }
        Ok(())
    }
}
