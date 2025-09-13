pub const fn rs(reg: u32) -> u32 {
    (reg & 0b11111) << 21
}

pub const fn rt(reg: u32) -> u32 {
    (reg & 0b11111) << 16
}

pub const fn rd(reg: u32) -> u32 {
    (reg & 0b11111) << 11
}

pub const fn opcode(op: u32) -> u32 {
    op << 26
}

pub const fn funct(func: u32) -> u32 {
    func & 0b111111
}

pub const fn shamt(shamt: u32) -> u32 {
    (shamt & 0b111111) << (6)
}

pub const fn imm_16(imm: u32) -> u32 {
    imm & 0xFFFF
}

pub const fn imm_26(imm: u32) -> u32 {
    imm & 0x3FFFFFF
}

#[repr(u32)]
#[derive(Clone, Copy, Debug)]
pub enum Opcodes {
    Add = opcode(0) | shamt(0) | funct(0b100000),
    Addu = opcode(0) | shamt(0) | funct(0b100001),
    Sub = opcode(0) | shamt(0) | funct(0b100010),
    Subu = opcode(0) | shamt(0) | funct(0b100011),
    And = opcode(0) | shamt(0) | funct(0b100100),
    Nor = opcode(0) | shamt(0) | funct(0b100111),
    Or = opcode(0) | shamt(0) | funct(0b100101),
    Xor = opcode(0) | shamt(0) | funct(0b100110),

    Addi = opcode(0b001000),
    Addiu = opcode(0b001001),
    Ori = opcode(0b001101),
    Xori = opcode(0b001110),
    Andi = opcode(0b001100),

    Sll = opcode(0b000000) | funct(0b000000),
    Sllv = opcode(0b000000) | funct(0b000100),
    Sra = opcode(0b000000) | funct(0b000011),
    Srav = opcode(0b000000) | funct(0b000111),
    Srl = opcode(0b000000) | funct(0b000010),
    Srlv = opcode(0b000000) | funct(0b000110),

    Slt = opcode(0b000000) | funct(0b101010),
    Sltu = opcode(0b000000) | funct(0b101011),
    Slti = opcode(0b001010),
    Sltiu = opcode(0b001011),

    Div = opcode(0b000000) | funct(0b011010),
    Divu = opcode(0b000000) | funct(0b011011),
    Mult = opcode(0b000000) | funct(0b011000),
    Multu = opcode(0b000000) | funct(0b011001),

    Mfhi = opcode(0b000000) | funct(0b010000),
    Mflo = opcode(0b000000) | funct(0b010010),
    Mthi = opcode(0b000000) | funct(0b010001),
    Mtlo = opcode(0b000000) | funct(0b010011),

    J = opcode(0b000010),
    Jal = opcode(0b000011),
    Jalr = opcode(0b000000) | funct(0b001001),
    Jr = opcode(0b000000) | funct(0b001000),

    Beq = opcode(0b000100),
    Bgez = opcode(0b000001) | rt(0b00001),
    Bgezal = opcode(0b000001) | rt(0b10001),
    Bgtz = opcode(0b000111) | rt(0b00000),
    Blez = opcode(0b000110) | rt(0b00000),
    Bltz = opcode(0b000001) | rt(0b00000),
    Bltzal = opcode(0b000001) | rt(0b10000),
    Bne = opcode(0b000101),

    Lb = opcode(0b100000),
    Lbu = opcode(0b100100),
    Lh = opcode(0b100001),
    Lhu = opcode(0b100101),
    Lw = opcode(0b100011),
    Lwl = opcode(0b100010),
    Lwr = opcode(0b100110),

    Sb = opcode(0b101000),
    Sh = opcode(0b101001),
    Sw = opcode(0b101011),
    Swl = opcode(0b101010),
    Swr = opcode(0b101110),

    Lui = opcode(0b001111),

    Break = opcode(0b000000) | funct(0b001101),
    Syscall = opcode(0b000000) | funct(0b001100),
}
