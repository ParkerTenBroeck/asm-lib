#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogOn {
    Error,
    Warning,
    None,
}

impl LogOn {
    pub fn supress(self, supress: bool) -> Self {
        if supress { LogOn::None } else { self }
    }

    pub fn is_error(self) -> bool {
        matches!(self, Self::Error)
    }

    pub fn is_warning(self) -> bool {
        matches!(self, Self::Warning)
    }

    pub fn is_none(self) -> bool {
        matches!(self, Self::None)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CastConfig {
    pub narrowing: LogOn,
    pub widening: LogOn,
    pub sign: LogOn,
    pub lossy: LogOn,
    pub f2i: LogOn,
    pub i2f: LogOn,
    pub changed_identity: LogOn,
    pub changed: LogOn,
}

#[derive(Debug)]
pub struct AssemblerConfig {
    pub preprocessor_stack_limit: usize,
    pub pic: bool,

    pub implicit_casts_ints: CastConfig,
    pub implicit_casts_ints_ptr: CastConfig,
    pub implicit_casts_ints_size: CastConfig,
    pub implicit_casts_non_numeric: CastConfig,

    pub implicit_cast_label_offset: CastConfig,
    pub implicit_cast_shift_value: CastConfig,

    pub emit_line_debug: bool,
    pub emit_reloc_debug: bool,
    pub emit_symbol_debug: bool,
}

impl AssemblerConfig {
    pub fn new() -> Self {
        Self {
            preprocessor_stack_limit: 100,
            pic: true,
            implicit_casts_ints: CastConfig {
                narrowing: LogOn::Error,
                widening: LogOn::Error,
                sign: LogOn::Error,
                lossy: LogOn::Error,
                f2i: LogOn::Error,
                i2f: LogOn::Warning,
                changed_identity: LogOn::Error,
                changed: LogOn::Error,
            },
            implicit_casts_ints_ptr: CastConfig {
                narrowing: LogOn::Error,
                widening: LogOn::Error,
                sign: LogOn::Error,
                lossy: LogOn::Error,
                f2i: LogOn::Error,
                i2f: LogOn::Error,
                changed_identity: LogOn::None,
                changed: LogOn::Error,
            },
            implicit_casts_ints_size: CastConfig {
                narrowing: LogOn::Error,
                widening: LogOn::Error,
                sign: LogOn::Error,
                lossy: LogOn::Error,
                f2i: LogOn::Error,
                i2f: LogOn::Error,
                changed_identity: LogOn::None,
                changed: LogOn::Error,
            },

            implicit_casts_non_numeric: CastConfig {
                narrowing: LogOn::Error,
                widening: LogOn::Error,
                sign: LogOn::Error,
                lossy: LogOn::Error,
                f2i: LogOn::Error,
                i2f: LogOn::Error,
                changed_identity: LogOn::Error,
                changed: LogOn::Error,
            },

            implicit_cast_label_offset: CastConfig {
                narrowing: LogOn::Warning,
                widening: LogOn::Warning,
                sign: LogOn::None,

                lossy: LogOn::Warning,
                f2i: LogOn::Error,
                i2f: LogOn::Error,
                changed_identity: LogOn::None,
                changed: LogOn::Error,
            },
            implicit_cast_shift_value: CastConfig {
                narrowing: LogOn::None,
                widening: LogOn::None,
                sign: LogOn::None,
                lossy: LogOn::None,
                f2i: LogOn::Error,
                i2f: LogOn::Error,
                changed: LogOn::Error,
                changed_identity: LogOn::Error,
            },
            emit_line_debug: true,
            emit_reloc_debug: true,
            emit_symbol_debug: true,
        }
    }
}

impl Default for AssemblerConfig {
    fn default() -> Self {
        Self::new()
    }
}
