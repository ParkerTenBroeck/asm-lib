use std::{
    fmt::{Display, Formatter},
    str::FromStr,
};

use assembler::expression::AssemblyRegister;

use crate::{MipsAssembler, opcodes::*};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, Default)]
pub struct Register(pub u8);

impl Register {
    pub fn rd(&self) -> u32 {
        rd(self.0 as u32)
    }
    pub fn rt(&self) -> u32 {
        rt(self.0 as u32)
    }
    pub fn rs(&self) -> u32 {
        rs(self.0 as u32)
    }
}
pub struct InvalidRegisterName;

impl FromStr for Register {
    type Err = InvalidRegisterName;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "x0" | "zero" => Ok(Self(0)),
            "x1" | "at" => Ok(Self(1)),
            "x2" | "v0" => Ok(Self(2)),
            "x3" | "v1" => Ok(Self(3)),
            "x4" | "a0" => Ok(Self(4)),
            "x5" | "a1" => Ok(Self(5)),
            "x6" | "a2" => Ok(Self(6)),
            "x7" | "a3" => Ok(Self(7)),
            "x8" | "t0" => Ok(Self(8)),
            "x9" | "t1" => Ok(Self(9)),
            "x10" | "t2" => Ok(Self(10)),
            "x11" | "t3" => Ok(Self(11)),
            "x12" | "t4" => Ok(Self(12)),
            "x13" | "t5" => Ok(Self(13)),
            "x14" | "t6" => Ok(Self(14)),
            "x15" | "t7" => Ok(Self(15)),
            "x16" | "s0" => Ok(Self(16)),
            "x17" | "s1" => Ok(Self(17)),
            "x18" | "s2" => Ok(Self(18)),
            "x19" | "s3" => Ok(Self(19)),
            "x20" | "s4" => Ok(Self(20)),
            "x21" | "s5" => Ok(Self(21)),
            "x22" | "s6" => Ok(Self(22)),
            "x23" | "s7" => Ok(Self(23)),
            "x24" | "t8" => Ok(Self(24)),
            "x25" | "t9" => Ok(Self(25)),
            "x26" | "k9" => Ok(Self(26)),
            "x27" | "k1" => Ok(Self(27)),
            "x28" | "gp" => Ok(Self(28)),
            "x29" | "sp" => Ok(Self(29)),
            "x30" | "s8" | "fp" => Ok(Self(30)),
            "x31" | "ra" => Ok(Self(31)),
            _ => Err(InvalidRegisterName),
        }
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let regs = [
            "zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "fp", "s1", "a0", "a1", "a2", "a3",
            "a4", "a5", "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11",
            "t3", "t4", "t5", "t6",
        ];
        match self.0 {
            0..32 => write!(f, "{}", regs[self.0 as usize]),
            _ => write!(f, "UNKNOWN<{}>", self.0),
        }
    }
}

impl<'a> AssemblyRegister<'a> for Register {
    type Lang = MipsAssembler<'a>;
}
