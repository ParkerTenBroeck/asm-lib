use num_traits::{NumCast, PrimInt};

#[derive(Clone, Debug)]
pub struct Data<T: PrimInt> {
    data: Vec<u8>,
    align: T,
    size: T,
}

impl<T: PrimInt> Default for Data<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[must_use]
pub enum PushDataResult<T> {
    Ok(T),
    WarningAlign(T),
    NotEnoughSpace,
}

impl<T> PushDataResult<T> {
    pub fn map<F>(self, map: impl FnOnce(T) -> F) -> PushDataResult<F> {
        match self {
            Self::Ok(val) => PushDataResult::Ok(map(val)),
            Self::WarningAlign(val) => PushDataResult::WarningAlign(map(val)),
            Self::NotEnoughSpace => PushDataResult::NotEnoughSpace,
        }
    }

    pub fn inspect(self, inspect: impl FnOnce(&T)) -> Self {
        match &self {
            PushDataResult::Ok(ok) => {
                inspect(ok);
            }
            PushDataResult::WarningAlign(warn) => {
                inspect(warn);
            }
            PushDataResult::NotEnoughSpace => {}
        }
        self
    }

    pub fn and_then<F>(self, then: impl FnOnce(T) -> PushDataResult<F>) -> PushDataResult<F> {
        match self {
            Self::Ok(val) => then(val),
            Self::WarningAlign(val) => match then(val) {
                PushDataResult::Ok(val) => PushDataResult::WarningAlign(val),
                r => r,
            },
            Self::NotEnoughSpace => PushDataResult::NotEnoughSpace,
        }
    }

    pub fn ignore_warning(self) -> Self {
        match self {
            Self::Ok(ok) => Self::Ok(ok),
            Self::WarningAlign(ok) => Self::Ok(ok),
            Self::NotEnoughSpace => Self::NotEnoughSpace,
        }
    }
}

impl<T: PrimInt> Data<T> {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            align: num_traits::one(),
            size: num_traits::zero(),
        }
    }

    pub fn slice(&self) -> &[u8] {
        &self.data
    }

    pub fn current_offset(&self) -> T {
        NumCast::from(self.data.len()).unwrap()
    }

    pub fn push_data(&mut self, data: &[u8], align: T) -> PushDataResult<std::ops::Range<T>> {
        let start = self.size;

        self.push_align(align).and_then(|_| {
            let Some(push) = T::from(data.len()) else {
                return PushDataResult::NotEnoughSpace;
            };
            if let Some(size) = self.size.checked_add(&push) {
                self.size = size;
            } else {
                return PushDataResult::NotEnoughSpace;
            }
            self.data.extend_from_slice(data);
            PushDataResult::Ok(start..self.size)
        })
    }

    pub fn push_space(&mut self, space: T, align: T) -> PushDataResult<std::ops::Range<T>> {
        let start = self.size;

        self.push_align(align).and_then(|_| {
            let Some(push) = T::from(space) else {
                return PushDataResult::NotEnoughSpace;
            };
            if let Some(size) = self.size.checked_add(&push) {
                self.size = size;
            } else {
                return PushDataResult::NotEnoughSpace;
            }
            let Some(size) = <usize as NumCast>::from(space) else {
                return PushDataResult::NotEnoughSpace;
            };
            self.data.resize(self.data.len() + size, 0);
            PushDataResult::Ok(start..self.size)
        })
    }

    pub fn push_align(&mut self, align: T) -> PushDataResult<std::ops::Range<T>> {
        let start = self.size;
        let mask = align.saturating_sub(num_traits::one());
        let push = (align - (self.size & mask)) & mask;
        if let Some(size) = self.size.checked_add(&push) {
            self.size = size;
        } else {
            return PushDataResult::NotEnoughSpace;
        }
        {
            let Some(push) = <usize as NumCast>::from(push) else {
                return PushDataResult::NotEnoughSpace;
            };
            self.data.resize(self.data.len() + push, 0);
        }
        self.align = self.align.max(align);
        if push.is_zero() {
            PushDataResult::Ok(start..self.size)
        } else {
            PushDataResult::WarningAlign(start..self.size)
        }
    }

    pub fn align(&self) -> T {
        self.align
    }

    pub fn size(&self) -> T {
        self.size
    }
}
