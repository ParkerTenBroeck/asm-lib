use num_traits::{NumCast, PrimInt};

#[derive(Clone, Debug)]
pub struct Data<T: PrimInt> {
    data: Vec<u8>,
    align: T,
}

impl<T: PrimInt> Default for Data<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: PrimInt> Data<T> {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            align: num_traits::one(),
        }
    }

    pub fn slice(&self) -> &[u8] {
        &self.data
    }

    pub fn current_offset(&self) -> T {
        NumCast::from(self.data.len()).unwrap()
    }

    pub fn push_data(&mut self, data: &[u8], align: T) -> std::ops::Range<usize> {
        let start = self.data.len();

        self.align = self.align.max(align);
        self.data.extend_from_slice(data);

        let end = self.data.len();

        start..end
    }

    pub fn push_space(&mut self, space: T, align: T) -> std::ops::Range<usize> {
        let start = self.data.len();

        self.align = self.align.max(align);
        let size: usize = NumCast::from(space).unwrap();
        self.data.resize(start + size, 0);

        let end = self.data.len();

        start..end
    }

    pub fn align(&self) -> T {
        self.align
    }
}
