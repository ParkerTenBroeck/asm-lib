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

    pub fn push_data(&mut self, data: &[u8], align: T) -> std::ops::Range<T> {
        let start = self.data.len();

        self.push_align(align);
        self.data.extend_from_slice(data);

        let end = self.data.len();

        T::from(start).unwrap()..T::from(end).unwrap()
    }

    pub fn push_space(&mut self, space: T, align: T) -> std::ops::Range<T> {
        let start = self.data.len();

        self.push_align(align);
        let size: usize = NumCast::from(space).unwrap();
        self.data.resize(start + size, 0);

        let end = self.data.len();

        T::from(start).unwrap()..T::from(end).unwrap()
    }

    pub fn push_align(&mut self, align: T) {
        self.align = self.align.max(align);
    }

    pub fn align(&self) -> T {
        self.align
    }

    pub fn size(&self) -> T {
        self.size
    }
}
