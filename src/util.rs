use std::fmt::{Display, Formatter};

pub trait IntoStrDelimable<T: Display>: Sized + Iterator<Item = T> {
    fn delim(self, delim: &str) -> StrDelimable<'_, T, Self> {
        StrDelimable { iter: self, delim }
    }
}

impl<T: Display, I: Iterator<Item = T>> IntoStrDelimable<T> for I {}

#[derive(Clone, Debug)]
pub struct StrDelimable<'a, T: Display, I: Iterator<Item = T>> {
    iter: I,
    delim: &'a str,
}

impl<'a, T: Display, I: Iterator<Item = T> + Clone> std::fmt::Display for StrDelimable<'a, T, I> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.iter.clone().peekable();
        while let Some(next) = iter.next() {
            write!(f, "{next}")?;
            if iter.peek().is_some() {
                write!(f, "{}", self.delim)?;
            }
        }
        Ok(())
    }
}
