use std::cmp::{Ord, Ordering};
use std::fs::File;
use std::io::Read;

struct Memory {
    buffer: Box<[u8]>,
}

/// The memory for our 8086 simulator
impl Memory {
    pub fn new() -> Memory {
        let buffer: Vec<u8> = vec![0u8; 1024 * 1024];
        let buffer = buffer.into_boxed_slice();
        Memory { buffer }
    }

    pub fn read_u8(&self, address: u16) -> u8 {
        let address: usize = address.try_into().unwrap();
        self.buffer[address]
    }

    pub fn get_address(&self, address: MemoryAddress) -> MemoryValue {
        MemoryValue::Byte(self.buffer[address.address])
    }

    pub fn set_address(&mut self, address: Address, value: MemoryValue) {
        match value {
            MemoryValue::Byte(value) => self.buffer[address.to_memory_address().address] = value,
            MemoryValue::Word(value) => {
                let address = address.to_memory_address().address;
                self.buffer[address] = (value >> 8) as u8;
                self.buffer[address + 1] = value as u8;
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum Address {
    Register(Register),
    EffectiveFormula(EffectiveAddress),
    MemoryAddress(MemoryAddress),
}

impl std::fmt::Display for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Register(register) => write!(f, "{}", register),
            Self::EffectiveFormula(formula) => write!(f, "{}", formula),
            Self::MemoryAddress(address) => write!(f, "{}", address),
        }
    }
}

impl Address {
    pub fn to_memory_address(self) -> MemoryAddress {
        match self {
            Self::Register(register) => {
                panic!("Can't get memory address for register: {}", register)
            }
            Self::MemoryAddress(address) => address,
            Self::EffectiveFormula(effective_address) => MemoryAddress {
                address: effective_address.to_memory_address(),
            },
        }
    }
}

#[derive(Debug, Copy, Clone)]
struct MemoryAddress {
    address: usize,
}

impl std::convert::From<MemoryValue> for MemoryAddress {
    fn from(value: MemoryValue) -> Self {
        Self {
            address: match value {
                MemoryValue::Byte(value) => value as usize,
                MemoryValue::Word(value) => value as usize,
            },
        }
    }
}

impl std::fmt::Display for MemoryAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.address)
    }
}

#[derive(Debug, Copy, Clone)]
enum EffectiveAddress {
    BxSi(u16),
    BxDi(u16),
    BpSi(u16),
    BpDi(u16),
    Si(u16),
    Di(u16),
    Bp(u16),
    Bx(u16),
}

impl std::fmt::Display for EffectiveAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::BxSi(displacement) => write!(f, "[bx+si+{}]", displacement),
            Self::BxDi(displacement) => write!(f, "[bx+di+{}]", displacement),
            Self::BpSi(displacement) => write!(f, "[bp+si+{}]", displacement),
            Self::BpDi(displacement) => write!(f, "[bp+di+{}]", displacement),
            Self::Si(displacement) => write!(f, "[si+{}]", displacement),
            Self::Di(displacement) => write!(f, "[di+{}]", displacement),
            Self::Bp(displacement) => write!(f, "[bp+{}]", displacement),
            Self::Bx(displacement) => write!(f, "[bx+{}]", displacement),
        }
    }
}

impl EffectiveAddress {
    pub fn from_str(effective_address_formula: String, displacement: u16) -> EffectiveAddress {
        match effective_address_formula.as_str() {
            "bx+si" => EffectiveAddress::BxSi(displacement),
            "bx+di" => EffectiveAddress::BxDi(displacement),
            "bp+si" => EffectiveAddress::BpSi(displacement),
            "bp+di" => EffectiveAddress::BpDi(displacement),
            "si" => EffectiveAddress::Si(displacement),
            "di" => EffectiveAddress::Di(displacement),
            "bp" => EffectiveAddress::Bp(displacement),
            "bx" => EffectiveAddress::Bx(displacement),
            _ => panic!(
                "Invalid effective address formula: {}",
                effective_address_formula
            ),
        }
    }

    fn to_memory_address(self) -> usize {
        todo!()
    }
}

pub struct Cpu {
    memory: Memory,
    registers: Registers,
    last_instruction: usize,
}

#[derive(Clone, Copy, Debug)]
enum Flag {
    Af,
    Cf,
    Of,
    Sf,
    Pf,
    Zf,
    Df,
    If,
    Tf,
}

impl std::fmt::Display for Flag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Af => write!(f, "A"),
            Self::Cf => write!(f, "C"),
            Self::Of => write!(f, "O"),
            Self::Sf => write!(f, "S"),
            Self::Pf => write!(f, "P"),
            Self::Zf => write!(f, "Z"),
            Self::Df => write!(f, "D"),
            Self::If => write!(f, "I"),
            Self::Tf => write!(f, "T"),
        }
    }
}

#[derive(Copy, Clone, Debug)]
struct Registers {
    general: [u16; 8], // 8 word sized registers
    segment: [u16; 4], // 4 word sized registers
    instruction_pointer: u16,
    flags: u16,
}

impl Registers {
    pub fn new() -> Registers {
        let general_registers = [0u16; 8];

        let segment_registers = [0u16; 4];

        Registers {
            general: general_registers,
            segment: segment_registers,
            instruction_pointer: 0,
            flags: 0,
        }
    }

    pub fn code_segment(&self) -> u16 {
        self.segment[0]
    }

    pub fn increment_instruction_pointer(&mut self) {
        self.instruction_pointer += 1
    }

    fn set_high_byte(byte: &mut u16, value: u8) {
        *byte &= 0x00FF; // clear high byte
        *byte |= (value as u16) << 8; // set high byte
    }

    fn set_low_byte(byte: &mut u16, value: u8) {
        *byte &= 0xFF00; // clear low byte
        *byte |= value as u16; // set low byte
    }

    pub fn get_flag(&self, flag: Flag) -> bool {
        match flag {
            Flag::Af => (self.flags & 0x0010) != 0,
            Flag::Cf => (self.flags & 0x0001) != 0,
            Flag::Of => (self.flags & 0x0800) != 0,
            Flag::Sf => (self.flags & 0x0080) != 0,
            Flag::Pf => (self.flags & 0x0004) != 0,
            Flag::Zf => (self.flags & 0x0040) != 0,
            Flag::Df => (self.flags & 0x0400) != 0,
            Flag::If => (self.flags & 0x0200) != 0,
            Flag::Tf => (self.flags & 0x0100) != 0,
        }
    }

    fn set_flag(&mut self, flag: Flag, value: bool) {
        match flag {
            Flag::Af => {
                if value {
                    self.flags |= 0x0010
                } else {
                    self.flags &= 0xFFEF
                }
            }
            Flag::Cf => {
                if value {
                    self.flags |= 0x0001
                } else {
                    self.flags &= 0xFFFE
                }
            }
            Flag::Of => {
                if value {
                    self.flags |= 0x0800
                } else {
                    self.flags &= 0xF7FF
                }
            }
            Flag::Sf => {
                if value {
                    self.flags |= 0x0080
                } else {
                    self.flags &= 0xFF7F
                }
            }
            Flag::Pf => {
                if value {
                    self.flags |= 0x0004
                } else {
                    self.flags &= 0xFFFB
                }
            }
            Flag::Zf => {
                if value {
                    self.flags |= 0x0040
                } else {
                    self.flags &= 0xFFBF
                }
            }
            Flag::Df => {
                if value {
                    self.flags |= 0x0400
                } else {
                    self.flags &= 0xFBFF
                }
            }
            Flag::If => {
                if value {
                    self.flags |= 0x0200
                } else {
                    self.flags &= 0xFDFF
                }
            }
            Flag::Tf => {
                if value {
                    self.flags |= 0x0100
                } else {
                    self.flags &= 0xFEFF
                }
            }
        }
    }

    pub fn set_register(&mut self, register: Register, value: MemoryValue) {
        match (register, value) {
            (Register::Ax, MemoryValue::Word(value)) => self.general[0] = value,
            (Register::Bx, MemoryValue::Word(value)) => self.general[1] = value,
            (Register::Cx, MemoryValue::Word(value)) => self.general[2] = value,
            (Register::Dx, MemoryValue::Word(value)) => self.general[3] = value,
            (Register::Ah, MemoryValue::Byte(value)) => {
                Registers::set_high_byte(&mut self.general[0], value)
            }
            (Register::Bh, MemoryValue::Byte(value)) => {
                Registers::set_high_byte(&mut self.general[1], value)
            }
            (Register::Ch, MemoryValue::Byte(value)) => {
                Registers::set_high_byte(&mut self.general[2], value)
            }
            (Register::Dh, MemoryValue::Byte(value)) => {
                Registers::set_high_byte(&mut self.general[3], value)
            }
            (Register::Al, MemoryValue::Byte(value)) => {
                Registers::set_low_byte(&mut self.general[0], value)
            }
            (Register::Bl, MemoryValue::Byte(value)) => {
                Registers::set_low_byte(&mut self.general[1], value)
            }
            (Register::Cl, MemoryValue::Byte(value)) => {
                Registers::set_low_byte(&mut self.general[2], value)
            }
            (Register::Dl, MemoryValue::Byte(value)) => {
                Registers::set_low_byte(&mut self.general[3], value)
            }
            (Register::Sp, MemoryValue::Word(value)) => self.general[4] = value,
            (Register::Bp, MemoryValue::Word(value)) => self.general[5] = value,
            (Register::Si, MemoryValue::Word(value)) => self.general[6] = value,
            (Register::Di, MemoryValue::Word(value)) => self.general[7] = value,
            (Register::Cs, MemoryValue::Word(value)) => self.segment[0] = value,
            (Register::Ds, MemoryValue::Word(value)) => self.segment[1] = value,
            (Register::Ss, MemoryValue::Word(value)) => self.segment[2] = value,
            (Register::Es, MemoryValue::Word(value)) => self.segment[3] = value,
            (Register::Ip, MemoryValue::Word(value)) => self.instruction_pointer = value,
            (register, value) => panic!("Register {register:?} can't take a {value:?}"),
        }
    }

    pub fn get_register(&self, register: Register) -> MemoryValue {
        match register {
            Register::Ax => MemoryValue::Word(self.general[0]),
            Register::Bx => MemoryValue::Word(self.general[1]),
            Register::Cx => MemoryValue::Word(self.general[2]),
            Register::Dx => MemoryValue::Word(self.general[3]),
            Register::Ah => MemoryValue::Byte(self.general[0].to_be_bytes()[0]),
            Register::Bh => MemoryValue::Byte(self.general[1].to_be_bytes()[0]),
            Register::Ch => MemoryValue::Byte(self.general[2].to_be_bytes()[0]),
            Register::Dh => MemoryValue::Byte(self.general[3].to_be_bytes()[0]),
            Register::Al => MemoryValue::Byte(self.general[0].to_be_bytes()[1]),
            Register::Bl => MemoryValue::Byte(self.general[1].to_be_bytes()[1]),
            Register::Cl => MemoryValue::Byte(self.general[2].to_be_bytes()[1]),
            Register::Dl => MemoryValue::Byte(self.general[3].to_be_bytes()[1]),
            Register::Sp => MemoryValue::Word(self.general[4]),
            Register::Bp => MemoryValue::Word(self.general[5]),
            Register::Si => MemoryValue::Word(self.general[6]),
            Register::Di => MemoryValue::Word(self.general[7]),
            Register::Cs => MemoryValue::Word(self.segment[0]),
            Register::Ds => MemoryValue::Word(self.segment[1]),
            Register::Ss => MemoryValue::Word(self.segment[2]),
            Register::Es => MemoryValue::Word(self.segment[3]),
            Register::Ip => MemoryValue::Word(self.instruction_pointer),
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum Register {
    Ax,
    Bx,
    Cx,
    Dx,
    Ah,
    Bh,
    Ch,
    Dh,
    Al,
    Bl,
    Cl,
    Dl,
    Sp,
    Bp,
    Si,
    Di,
    Cs,
    Ds,
    Ss,
    Es,
    Ip,
}

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Register::Ax => write!(f, "ax"),
            Register::Bx => write!(f, "bx"),
            Register::Cx => write!(f, "cx"),
            Register::Dx => write!(f, "dx"),
            Register::Ah => write!(f, "ah"),
            Register::Bh => write!(f, "bh"),
            Register::Ch => write!(f, "ch"),
            Register::Dh => write!(f, "dh"),
            Register::Al => write!(f, "al"),
            Register::Bl => write!(f, "bl"),
            Register::Cl => write!(f, "cl"),
            Register::Dl => write!(f, "dl"),
            Register::Sp => write!(f, "sp"),
            Register::Si => write!(f, "si"),
            Register::Di => write!(f, "di"),
            Register::Ds => write!(f, "ds"),
            Register::Bp => write!(f, "bp"),
            Register::Cs => write!(f, "cs"),
            Register::Ss => write!(f, "ss"),
            Register::Es => write!(f, "es"),
            Register::Ip => write!(f, "ip"),
        }
    }
}

impl Register {
    pub fn from_str(register: &str) -> Register {
        match &register.to_lowercase()[..] {
            "ax" => Register::Ax,
            "bx" => Register::Bx,
            "cx" => Register::Cx,
            "dx" => Register::Dx,
            "al" => Register::Al,
            "bl" => Register::Bl,
            "cl" => Register::Cl,
            "dl" => Register::Dl,
            "ah" => Register::Ah,
            "bh" => Register::Bh,
            "ch" => Register::Ch,
            "dh" => Register::Dh,
            "sp" => Register::Sp,
            "bp" => Register::Bp,
            "si" => Register::Si,
            "di" => Register::Di,
            "cs" => Register::Cs,
            "ds" => Register::Ds,
            "ss" => Register::Ss,
            "es" => Register::Es,
            "ip" => Register::Ip,
            register => panic!("Unknown register: {register}"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
enum MemoryValue {
    Byte(u8),
    Word(u16),
}

impl MemoryValue {
    pub fn count_ones(&self) -> u32 {
        match self {
            MemoryValue::Byte(byte) => byte.count_ones(),
            MemoryValue::Word(word) => word.count_ones(),
        }
    }
}

impl std::convert::From<i32> for MemoryValue {
    fn from(value: i32) -> Self {
        MemoryValue::Word(value as u16)
    }
}

impl std::ops::Add for MemoryValue {
    type Output = MemoryValue;

    fn add(self, other: MemoryValue) -> MemoryValue {
        match (self, other) {
            (MemoryValue::Byte(a), MemoryValue::Byte(b)) => MemoryValue::Byte(a + b),
            (MemoryValue::Word(a), MemoryValue::Word(b)) => MemoryValue::Word(a + b),
            (MemoryValue::Byte(a), MemoryValue::Word(b)) => MemoryValue::Word(a as u16 + b),
            (MemoryValue::Word(a), MemoryValue::Byte(b)) => MemoryValue::Word(a + b as u16),
        }
    }
}

impl std::ops::Sub for MemoryValue {
    type Output = MemoryValue;

    fn sub(self, other: MemoryValue) -> MemoryValue {
        match (self, other) {
            (MemoryValue::Byte(a), MemoryValue::Byte(b)) => MemoryValue::Byte(a - b),
            (MemoryValue::Word(a), MemoryValue::Word(b)) => MemoryValue::Word(a - b),
            (MemoryValue::Byte(a), MemoryValue::Word(b)) => MemoryValue::Word(a as u16 - b),
            (MemoryValue::Word(a), MemoryValue::Byte(b)) => MemoryValue::Word(a - b as u16),
        }
    }
}

impl std::ops::BitAnd for MemoryValue {
    type Output = MemoryValue;

    fn bitand(self, other: MemoryValue) -> MemoryValue {
        match (self, other) {
            (MemoryValue::Byte(a), MemoryValue::Byte(b)) => MemoryValue::Byte(a & b),
            (MemoryValue::Word(a), MemoryValue::Word(b)) => MemoryValue::Word(a & b),
            (MemoryValue::Byte(a), MemoryValue::Word(b)) => MemoryValue::Word(a as u16 & b),
            (MemoryValue::Word(a), MemoryValue::Byte(b)) => MemoryValue::Word(a & b as u16),
        }
    }
}

impl std::ops::BitOr for MemoryValue {
    type Output = MemoryValue;

    fn bitor(self, other: MemoryValue) -> MemoryValue {
        match (self, other) {
            (MemoryValue::Byte(a), MemoryValue::Byte(b)) => MemoryValue::Byte(a | b),
            (MemoryValue::Word(a), MemoryValue::Word(b)) => MemoryValue::Word(a | b),
            (MemoryValue::Byte(a), MemoryValue::Word(b)) => MemoryValue::Word(a as u16 | b),
            (MemoryValue::Word(a), MemoryValue::Byte(b)) => MemoryValue::Word(a | b as u16),
        }
    }
}

impl std::fmt::LowerHex for MemoryValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            MemoryValue::Byte(value) => write!(f, "{:02x}", value),
            MemoryValue::Word(value) => write!(f, "{:04x}", value),
        }
    }
}

impl std::fmt::Display for MemoryValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            MemoryValue::Byte(value) => write!(f, "{:#}", value),
            MemoryValue::Word(value) => write!(f, "{:#}", value),
        }
    }
}

impl Default for Cpu {
    fn default() -> Self {
        Self::new()
    }
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            memory: Memory::new(),
            registers: Registers::new(),
            last_instruction: 0,
        }
    }

    pub fn read_file_into_memory(
        &mut self,
        f: &mut File,
        offset: usize,
    ) -> Result<usize, std::io::Error> {
        let read_len = f.read(&mut self.memory.buffer[offset..])?;
        self.last_instruction = offset + read_len;
        Ok(read_len)
    }

    pub fn next_instruction(&mut self) -> Option<u8> {
        if let MemoryValue::Word(ip) = self.registers.get_register(Register::Ip) {
            let next_instruction = self.memory.read_u8(self.registers.code_segment() * 64 + ip);
            self.registers.increment_instruction_pointer();
            if self.last_instruction < self.registers.instruction_pointer.try_into().unwrap() {
                None
            } else {
                Some(next_instruction)
            }
        } else {
            None
        }
    }

    pub fn run(&mut self) {
        while let Some(byte) = self.next_instruction() {
            let instruction = get_instruction(byte).unwrap();

            let instruction = match instruction {
                "mov-reg_mem-to_seg-reg" => mov_seg_reg_to_from_reg_mem(self, true),
                "mov-seg-reg-to-reg_mem" => mov_seg_reg_to_from_reg_mem(self, false),
                "mov-reg_mem-to_from-reg" => mov_reg_mem_to_from_reg(byte, self),
                "mov-immediate-to-reg" => mov_immediate_to_reg(byte, self),
                "mov-immediate-to-reg_mem" => mov_immediate_to_reg_mem(byte, self),
                "mov-memory-to-accumulator" => mov_memory_to_accumulator(byte, self),
                "mov-accumulator-to-memory" => mov_accumulator_to_memory(byte, self),
                "arithmatic-reg_mem-and-reg-to-either" => {
                    arithmatic_reg_mem_and_reg_to_either(byte, self)
                }
                "arithmatic-immediate-to-reg_mem" => arithmatic_immediate_to_reg_mem(byte, self),
                "arithmatic-immediate-with-accumulator" => {
                    arithmatic_immediate_with_accumulator(byte, self)
                }
                "jump" => jump(byte, self),
                "loop" => r#loop(byte, self),
                &_ => todo!("Not yet implemented {instruction}"),
            };

            self.execute(&instruction);
        }
        println!("\nFinal registers:");
        self.print_registers();
    }

    pub fn execute(&mut self, instruction: &Instruction) {
        //print_comment(format!("Going to execute instruction: {instruction:?}"));
        print!("{}", instruction.str_rep);
        let pre_op_registers = self.registers;
        match &instruction.op {
            Operation::MoveMemoryToAccumulator => {
                todo!()
            }
            Operation::MoveAccumulatorToMemory => {
                todo!()
            }
            Operation::MovImmediateToRegister => {
                match (&instruction.operands.left, &instruction.operands.right) {
                    (Operand::Register(register), Operand::ImmediateWord(value)) => {
                        self.registers
                            .set_register(*register, MemoryValue::Word(*value));
                    }
                    (Operand::Register(register), Operand::ImmediateByte(value)) => {
                        self.registers
                            .set_register(*register, MemoryValue::Byte(*value));
                    }
                    (left, right) => {
                        panic!(
                            "{:?} not defined for operands: ({left:?}, {right:?})",
                            instruction.op
                        )
                    }
                }
            }
            Operation::MovRegisterMemoryToFromRegister => {
                match (&instruction.operands.left, &instruction.operands.right) {
                    (
                        Operand::Register(dest_register),
                        Operand::Address(Address::Register(source_register)),
                    ) => {
                        self.registers.set_register(
                            *dest_register,
                            self.registers.get_register(*source_register),
                        );
                    }
                    (Operand::Register(register), Operand::Address(effective_address)) => {
                        self.registers.set_register(
                            *register,
                            self.memory
                                .get_address(effective_address.to_memory_address()),
                        );
                    }
                    (
                        Operand::Address(Address::Register(dest_register)),
                        Operand::Register(source_register),
                    ) => {
                        self.registers.set_register(
                            *dest_register,
                            self.registers.get_register(*source_register),
                        );
                    }
                    (Operand::Address(effective_address), Operand::Register(register)) => {
                        self.memory.set_address(
                            *effective_address,
                            self.registers.get_register(*register),
                        );
                    }
                    (left, right) => {
                        panic!(
                            "{:?} not defined for operands: ({left:?}, {right:?})",
                            instruction.op
                        )
                    }
                }
            }
            Operation::MoveImmediateToRegisterOrMemory(_wide) => {
                todo!()
            }
            Operation::Jump(_jump_type) => {
                todo!()
            }
            Operation::Loop(_loop_type) => {
                todo!()
            }
            Operation::ArithmaticRegisterOrMemoryAndRegisterToEither(arithmatic_type) => {
                let (dest, source) = match (&instruction.operands.left, &instruction.operands.right)
                {
                    (Operand::Register(register), Operand::Address(address)) => {
                        (Address::Register(*register), *address)
                    }
                    (Operand::Address(address), Operand::Register(register)) => {
                        (*address, Address::Register(*register))
                    }
                    _ => panic!(
                        "Invalid operands for arithmatic operation {:?}",
                        instruction.operands
                    ),
                };

                match arithmatic_type {
                    ArithmaticType::Add => {
                        self.set_address(dest, self.get_address(dest) + self.get_address(source))
                    }
                    ArithmaticType::Adc => {
                        todo!()
                    }
                    ArithmaticType::Or => {
                        todo!()
                    }
                    ArithmaticType::Sbb => {
                        todo!()
                    }
                    ArithmaticType::And => {
                        todo!()
                    }
                    ArithmaticType::Sub => {
                        let result = self.get_address(dest) - self.get_address(source);
                        self.set_address(dest, result);

                        // Set Flag::Sf if the most significant bit is set in result
                        self.registers
                            .set_flag(Flag::Sf, result & 0x8000.into() != 0.into());
                    }
                    ArithmaticType::Xor => {
                        todo!()
                    }
                    ArithmaticType::Cmp => {
                        match self
                            .get_address(dest)
                            .partial_cmp(&self.get_address(source))
                        {
                            Some(Ordering::Less) => {
                                self.registers.set_flag(Flag::Sf, true);
                                self.registers.set_flag(Flag::Zf, false);
                                self.registers.set_flag(Flag::Cf, true);
                            }
                            Some(Ordering::Equal) => {
                                self.registers.set_flag(Flag::Sf, false);
                                self.registers.set_flag(Flag::Zf, true);
                                self.registers.set_flag(Flag::Cf, false);
                            }
                            Some(Ordering::Greater) => {
                                self.registers.set_flag(Flag::Sf, false);
                                self.registers.set_flag(Flag::Zf, false);
                                self.registers.set_flag(Flag::Cf, false);
                            }
                            None => todo!(),
                        }
                    }
                }
            }
            Operation::ArithmaticImmediateToRegisterOrMemory(arithmatic_type, _wide) => {
                let (dest, immediate) =
                    match (&instruction.operands.left, &instruction.operands.right) {
                        (
                            Operand::Address(Address::Register(register)),
                            Operand::ImmediateWord(immediate),
                        ) => (Address::Register(*register), MemoryValue::Word(*immediate)),
                        _ => panic!(
                            "Invalid operands for arithmatic operation {:?}",
                            instruction.operands
                        ),
                    };

                let (result, update_dest) = match arithmatic_type {
                    ArithmaticType::Add => (self.get_address(dest) + immediate, true),
                    ArithmaticType::Adc => {
                        todo!()
                    }
                    ArithmaticType::Or => {
                        todo!()
                    }
                    ArithmaticType::Sbb => {
                        todo!()
                    }
                    ArithmaticType::And => {
                        todo!()
                    }
                    ArithmaticType::Sub => (self.get_address(dest) - immediate, true),
                    ArithmaticType::Xor => {
                        todo!()
                    }
                    ArithmaticType::Cmp => (0.into(), false),
                };

                if update_dest {
                    self.set_address(dest, result);
                }

                match arithmatic_type {
                    ArithmaticType::Sub | ArithmaticType::Cmp => {
                        match self.get_address(dest).partial_cmp(&immediate) {
                            Some(Ordering::Less) => {
                                self.registers.set_flag(Flag::Sf, true);
                                self.registers.set_flag(Flag::Zf, false);
                                self.registers.set_flag(Flag::Cf, true);
                            }
                            Some(Ordering::Equal) => {
                                self.registers.set_flag(Flag::Sf, false);
                                self.registers.set_flag(Flag::Zf, true);
                                self.registers.set_flag(Flag::Cf, false);
                            }
                            Some(Ordering::Greater) => {
                                self.registers.set_flag(Flag::Sf, false);
                                self.registers.set_flag(Flag::Zf, false);
                                self.registers.set_flag(Flag::Cf, false);
                            }
                            None => todo!(),
                        }
                    }
                    _ => {}
                }

                match arithmatic_type {
                    ArithmaticType::Sub | ArithmaticType::Cmp | ArithmaticType::Add => {
                        // Set Flag::Pf if the result has even parity.
                        self.registers
                            .set_flag(Flag::Pf, result.count_ones() % 2 == 0);
                    }
                    _ => {}
                }
            }
            Operation::ArithmaticImmediateWithAccumulator => {
                todo!()
            }
        }
        print!("\t; ");
        let post_op_registers = self.registers;
        for register in Self::DEBUG_REGISTERS {
            register_changed(register, pre_op_registers, post_op_registers);
        }
        let pre_flags: String = Self::DEBUG_FLAGS
            .iter()
            .filter(|f| pre_op_registers.get_flag(**f))
            .map(|f| format!("{}", f))
            .collect();
        let post_flags: String = Self::DEBUG_FLAGS
            .iter()
            .filter(|f| post_op_registers.get_flag(**f))
            .map(|f| format!("{}", f))
            .collect();
        if pre_flags != post_flags {
            print!(" flags:{}->{}", pre_flags, post_flags);
        }
        println!();
    }

    const DEBUG_FLAGS: [Flag; 9] = [
        Flag::Sf,
        Flag::Zf,
        Flag::Af,
        Flag::Pf,
        Flag::Cf,
        Flag::Of,
        Flag::Df,
        Flag::If,
        Flag::Tf,
    ];

    fn set_address(&mut self, address: Address, value: MemoryValue) {
        match address {
            Address::Register(register) => self.registers.set_register(register, value),
            Address::MemoryAddress(_) => self.memory.set_address(address, value),
            Address::EffectiveFormula(effective_formula) => self
                .memory
                .set_address(self.calculate_address(effective_formula), value),
        }
    }

    fn get_address(&self, address: Address) -> MemoryValue {
        match address {
            Address::Register(register) => self.registers.get_register(register),
            Address::MemoryAddress(address) => self.memory.get_address(address),
            Address::EffectiveFormula(effective_formula) => {
                self.get_address(self.calculate_address(effective_formula))
            }
        }
    }

    fn calculate_address(&self, effective_address_formula: EffectiveAddress) -> Address {
        Address::MemoryAddress(match effective_address_formula {
            EffectiveAddress::BxSi(displacement) => (self.registers.get_register(Register::Bx)
                + self.registers.get_register(Register::Si)
                + MemoryValue::Word(displacement))
            .into(),
            EffectiveAddress::BxDi(displacement) => (self.registers.get_register(Register::Bx)
                + self.registers.get_register(Register::Di)
                + MemoryValue::Word(displacement))
            .into(),
            EffectiveAddress::BpSi(displacement) => (self.registers.get_register(Register::Bp)
                + self.registers.get_register(Register::Si)
                + MemoryValue::Word(displacement))
            .into(),
            EffectiveAddress::BpDi(displacement) => (self.registers.get_register(Register::Bp)
                + self.registers.get_register(Register::Di)
                + MemoryValue::Word(displacement))
            .into(),
            EffectiveAddress::Si(displacement) => {
                (self.registers.get_register(Register::Si) + MemoryValue::Word(displacement)).into()
            }
            EffectiveAddress::Di(displacement) => {
                (self.registers.get_register(Register::Di) + MemoryValue::Word(displacement)).into()
            }
            EffectiveAddress::Bp(displacement) => {
                (self.registers.get_register(Register::Bp) + MemoryValue::Word(displacement)).into()
            }
            EffectiveAddress::Bx(displacement) => {
                (self.registers.get_register(Register::Bx) + MemoryValue::Word(displacement)).into()
            }
        })
    }

    const DEBUG_REGISTERS: [Register; 13] = [
        Register::Ax,
        Register::Bx,
        Register::Cx,
        Register::Dx,
        Register::Sp,
        Register::Bp,
        Register::Si,
        Register::Di,
        Register::Cs,
        Register::Ds,
        Register::Ss,
        Register::Es,
        Register::Ip,
    ];

    fn print_registers(&self) {
        for register in Self::DEBUG_REGISTERS {
            let value = self.registers.get_register(register);
            println!("\t{register}: 0x{value:04x} ({value})",);
        }
        let flags: String = Self::DEBUG_FLAGS
            .iter()
            .filter(|f| self.registers.get_flag(**f))
            .map(|f| format!("{}", f))
            .collect();
        println!("\tflags: {flags}");
    }
}

fn register_changed(register: Register, pre: Registers, post: Registers) {
    let pre = pre.get_register(register);
    let post = post.get_register(register);
    if pre != post {
        print!("{}:0x{:04x}->0x{:04x}", register, pre, post);
    }
}

#[derive(Debug)]
pub struct Instruction {
    op: Operation,
    operands: Operands,
    str_rep: String,
}

#[derive(Debug)]
struct Operands {
    left: Operand,
    right: Operand,
}

#[derive(Debug)]
enum Operation {
    MovImmediateToRegister,
    MovRegisterMemoryToFromRegister,
    MoveImmediateToRegisterOrMemory(bool),
    MoveMemoryToAccumulator,
    MoveAccumulatorToMemory,
    ArithmaticImmediateWithAccumulator,
    Loop(LoopType),
    Jump(JumpType),
    ArithmaticRegisterOrMemoryAndRegisterToEither(ArithmaticType),
    ArithmaticImmediateToRegisterOrMemory(ArithmaticType, bool),
}

#[derive(Debug, Copy, Clone)]
enum ArithmaticType {
    Add,
    Or,
    Adc,
    Sbb,
    And,
    Sub,
    Xor,
    Cmp,
}

impl std::fmt::Display for ArithmaticType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ArithmaticType::Add => write!(f, "add"),
            ArithmaticType::Or => write!(f, "or"),
            ArithmaticType::Adc => write!(f, "adc"),
            ArithmaticType::Sbb => write!(f, "sbb"),
            ArithmaticType::And => write!(f, "and"),
            ArithmaticType::Sub => write!(f, "sub"),
            ArithmaticType::Xor => write!(f, "xor"),
            ArithmaticType::Cmp => write!(f, "cmp"),
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum JumpType {
    Je,
    Jl,
    Jle,
    Jb,
    Jbe,
    Jp,
    Jo,
    Js,
    Jne,
    Jnl,
    Jnle,
    Jnb,
    Jnbe,
    Jnp,
    Jno,
    Jns,
}

#[derive(Debug, Copy, Clone)]
enum LoopType {
    Loop,
    Loopz,
    Loopnz,
    Jcxz,
}

#[derive(Debug)]
enum Operand {
    Register(Register),
    ImmediateByte(u8),
    ImmediateWord(u16),
    Address(Address),
    SignedInstructionPointerIncrement(i8),
    None,
}

const OPCODES: [(u8, u8, &str); 14] = [
    (0b1000_1000, 0b1111_1100, "mov-reg_mem-to_from-reg"),
    (0b1000_1110, 0b1111_1111, "mov-reg_mem-to_seg-reg"),
    (0b1000_1100, 0b1111_1111, "mov-seg-reg-to-reg_mem"),
    (0b1011_0000, 0b1111_0000, "mov-immediate-to-reg"),
    (0b1100_0110, 0b1111_1110, "mov-immediate-to-reg_mem"),
    (0b1010_0000, 0b1111_1110, "mov-memory-to-accumulator"),
    (0b1010_0010, 0b1111_1110, "mov-accumulator-to-memory"),
    (
        0b0000_0000,
        0b1100_0100,
        "arithmatic-reg_mem-and-reg-to-either",
    ),
    (0b1000_0000, 0b1111_1100, "arithmatic-immediate-to-reg_mem"),
    (
        0b0000_0100,
        0b1100_0110,
        "arithmatic-immediate-with-accumulator",
    ),
    (
        0b0011_0100,
        0b1111_1100,
        "arithmatic-immediate-with reg_mem",
    ), // xor
    (
        0b0011_0100,
        0b1111_1110,
        "arithmatic-immediate-with-accumulator",
    ), // xor
    (0b0111_0000, 0b1111_0000, "jump"), // 16
    (0b1110_0000, 0b1111_1100, "loop"), // 4
];

fn get_instruction(byte: u8) -> Option<&'static str> {
    for (opcode, mask, instruction) in OPCODES {
        if (byte & mask) == opcode {
            return Some(instruction);
        }
    }

    panic!("Couldn't find instruction for byte {byte:#b}");
}

const REGISTER_ENCODING: [(&str, &str); 8] = [
    ("al", "ax"),
    ("cl", "cx"),
    ("dl", "dx"),
    ("bl", "bx"),
    ("ah", "sp"),
    ("ch", "bp"),
    ("dh", "si"),
    ("bh", "di"),
];

fn get_register_encoding(reg: u8, word: bool) -> &'static str {
    let reg: usize = reg.try_into().unwrap();
    if word {
        REGISTER_ENCODING[reg].1
    } else {
        REGISTER_ENCODING[reg].0
    }
}

const EFFECTIVE_ADDRESS: [&str; 8] = ["bx+si", "bx+di", "bp+si", "bp+di", "si", "di", "bp", "bx"];

fn get_effective_address(rm: usize) -> &'static str {
    EFFECTIVE_ADDRESS[rm]
}

const LOOP_OPS: [LoopType; 4] = [
    LoopType::Loop,
    LoopType::Loopz,
    LoopType::Loopnz,
    LoopType::Jcxz,
];

const JUMP_OPS: [JumpType; 16] = [
    JumpType::Je,
    JumpType::Jl,
    JumpType::Jle,
    JumpType::Jb,
    JumpType::Jbe,
    JumpType::Jp,
    JumpType::Jo,
    JumpType::Js,
    JumpType::Jne,
    JumpType::Jnl,
    JumpType::Jnle,
    JumpType::Jnb,
    JumpType::Jnbe,
    JumpType::Jnp,
    JumpType::Jno,
    JumpType::Jns,
];

fn r#loop(byte: u8, cpu: &mut Cpu) -> Instruction {
    let data: Vec<u8> = vec![cpu.next_instruction().unwrap()];

    let loop_op = LOOP_OPS[<u8 as TryInto<usize>>::try_into(byte & 3).unwrap()];

    let ip_inc8 = i8::from_ne_bytes(data[0..1].try_into().unwrap()) + 2;

    let inc_ip = match ip_inc8.cmp(&0) {
        Ordering::Less => format!("${}+0", ip_inc8),
        Ordering::Equal => "$+0".to_string(),
        Ordering::Greater => format!("$+{}+0", ip_inc8),
    };

    Instruction {
        op: Operation::Loop(loop_op),
        operands: Operands {
            left: Operand::SignedInstructionPointerIncrement(ip_inc8),
            right: Operand::None,
        },
        str_rep: format!("{loop_op:?} {inc_ip}"),
    }
}

fn jump(byte: u8, cpu: &mut Cpu) -> Instruction {
    let data: Vec<u8> = vec![cpu.next_instruction().unwrap()];

    let jump_op = JUMP_OPS[<u8 as TryInto<usize>>::try_into(byte & 15).unwrap()];

    let ip_inc8 = i8::from_ne_bytes(data[0..1].try_into().unwrap()) + 2;

    let inc_ip = match ip_inc8.cmp(&0) {
        Ordering::Less => format!("${}+0", ip_inc8),
        Ordering::Equal => "$+0".to_string(),
        Ordering::Greater => format!("$+{}+0", ip_inc8),
    };

    Instruction {
        op: Operation::Jump(jump_op),
        operands: Operands {
            left: Operand::SignedInstructionPointerIncrement(ip_inc8),
            right: Operand::None,
        },
        str_rep: format!("{jump_op:?} {inc_ip}"),
    }
}

const ARITHMATIC_OPS: [ArithmaticType; 8] = [
    ArithmaticType::Add,
    ArithmaticType::Or,
    ArithmaticType::Adc,
    ArithmaticType::Sbb,
    ArithmaticType::And,
    ArithmaticType::Sub,
    ArithmaticType::Xor,
    ArithmaticType::Cmp,
];

fn get_arithmatic_op(byte: u8) -> ArithmaticType {
    let index: usize = byte.try_into().unwrap();
    ARITHMATIC_OPS[index]
}

fn arithmatic_reg_mem_and_reg_to_either(byte: u8, cpu: &mut Cpu) -> Instruction {
    let mut data: Vec<u8> = Vec::new();
    let reg_is_destination = byte & 0x2 == 0x2;
    let wide = byte & 0x1 == 0x1;
    //print_cmment("REG is destination: {reg_is_destination}, Operates on word: {w}");

    let op = get_arithmatic_op(byte >> 3 & 7);

    data.push(cpu.next_instruction().unwrap());
    let reg = get_register_encoding((data[0] & 0b0011_1000).rotate_right(3), wide);
    let effective_address_formula = get_effective_address_formula(&mut data, wide, cpu);
    if reg_is_destination {
        Instruction {
            op: Operation::ArithmaticRegisterOrMemoryAndRegisterToEither(op),
            operands: Operands {
                left: Operand::Register(Register::from_str(reg)),
                right: Operand::Address(effective_address_formula),
            },
            str_rep: format!("{op} {reg}, {effective_address_formula}"),
        }
    } else {
        Instruction {
            op: Operation::ArithmaticRegisterOrMemoryAndRegisterToEither(op),
            operands: Operands {
                left: Operand::Address(effective_address_formula),
                right: Operand::Register(Register::from_str(reg)),
            },
            str_rep: format!("{op} {effective_address_formula}, {reg}"),
        }
    }
}

const SEGMENT_REGISTERS: [Register; 4] = [
    Register::Es, // 00
    Register::Cs, // 01
    Register::Ss, // 10
    Register::Ds, // 11
];

/// Parses MOV instructions that use the segment registers.
///
fn mov_seg_reg_to_from_reg_mem(cpu: &mut Cpu, register_is_destination: bool) -> Instruction {
    let mut data = vec![cpu.next_instruction().unwrap()];

    let segment_register = SEGMENT_REGISTERS[(data[0] >> 3 & 3) as usize];

    let effective_address_formula = get_effective_address_formula(&mut data, true, cpu);

    if register_is_destination {
        Instruction {
            op: Operation::MovRegisterMemoryToFromRegister,
            operands: Operands {
                left: Operand::Register(segment_register),
                right: Operand::Address(effective_address_formula),
            },
            str_rep: format!("mov {segment_register}, {effective_address_formula}"),
        }
    } else {
        Instruction {
            op: Operation::MovRegisterMemoryToFromRegister,
            operands: Operands {
                left: Operand::Address(effective_address_formula),
                right: Operand::Register(segment_register),
            },
            str_rep: format!("mov {effective_address_formula}, {segment_register}"),
        }
    }
}

fn mov_reg_mem_to_from_reg(byte: u8, cpu: &mut Cpu) -> Instruction {
    let mut data: Vec<u8> = Vec::new();
    let reg_is_destination = byte & 0x2 == 0x2;
    let wide = byte & 0x1 == 0x1;
    //print_comment(format!(
    //    "REG is destination: {reg_is_destination}, Operates on word: {wide}"
    //));

    data.push(cpu.next_instruction().unwrap());
    let reg = get_register_encoding((data[0] & 0b0011_1000).rotate_right(3), wide);
    let effective_address_formula = get_effective_address_formula(&mut data, wide, cpu);
    if reg_is_destination {
        Instruction {
            op: Operation::MovRegisterMemoryToFromRegister,
            operands: Operands {
                left: Operand::Register(Register::from_str(reg)),
                right: Operand::Address(effective_address_formula),
            },
            str_rep: format!("mov {reg}, {effective_address_formula}"),
        }
    } else {
        Instruction {
            op: Operation::MovRegisterMemoryToFromRegister,
            operands: Operands {
                left: Operand::Address(effective_address_formula),
                right: Operand::Register(Register::from_str(reg)),
            },
            str_rep: format!("mov {effective_address_formula}, {reg}"),
        }
    }
}

fn arithmatic_immediate_to_reg_mem(byte: u8, cpu: &mut Cpu) -> Instruction {
    let mut data: Vec<u8> = Vec::new();
    let wide = byte & 1 == 1;
    let signed_extension = byte & 2 == 2;

    data.push(cpu.next_instruction().unwrap());

    let op = if (byte >> 3 & 7) == 6 {
        ArithmaticType::Xor
    } else {
        get_arithmatic_op(data[0] >> 3 & 7)
    };

    let effective_address_formula = get_effective_address_formula(&mut data, wide, cpu);

    data.push(cpu.next_instruction().unwrap());
    let immediate = if wide {
        if signed_extension {
            data.push(0);
        } else {
            data.push(cpu.next_instruction().unwrap());
        }

        let len = data.len();
        i16::from_ne_bytes(data[len - 2..len].try_into().unwrap())
    } else {
        let len = data.len();
        i8::from_ne_bytes(data[len - 1..len].try_into().unwrap()).into()
    };

    let width_specifier = if wide { "word " } else { "byte " };

    Instruction {
        str_rep: format!("{op} {width_specifier}{effective_address_formula}, {immediate}"),
        op: Operation::ArithmaticImmediateToRegisterOrMemory(op, wide),
        operands: Operands {
            left: Operand::Address(effective_address_formula),
            right: Operand::ImmediateWord(immediate as u16),
        },
    }
}

fn mov_immediate_to_reg_mem(byte: u8, cpu: &mut Cpu) -> Instruction {
    let mut data: Vec<u8> = Vec::new();
    let wide = byte & 1 == 1;
    //println!("; MOV immediate to register: {reg}, wide: {w}");

    data.push(cpu.next_instruction().unwrap());
    let effective_address_formula = get_effective_address_formula(&mut data, wide, cpu);

    data.push(cpu.next_instruction().unwrap());
    let immediate = if wide {
        data.push(cpu.next_instruction().unwrap());

        let len = data.len();
        i16::from_ne_bytes(data[len - 2..len].try_into().unwrap())
    } else {
        let len = data.len();
        i8::from_ne_bytes(data[len - 1..len].try_into().unwrap()).into()
    };

    Instruction {
        str_rep: format!("mov {effective_address_formula}, {immediate}"),
        op: Operation::MoveImmediateToRegisterOrMemory(wide),
        operands: Operands {
            left: Operand::Address(effective_address_formula),
            right: Operand::ImmediateWord(immediate as u16),
        },
    }
}

fn mov_accumulator_to_memory(byte: u8, cpu: &mut Cpu) -> Instruction {
    let mut data: Vec<u8> = Vec::new();
    let wide = byte & 1 == 1;

    data.push(cpu.next_instruction().unwrap());

    let address: u16 = if wide {
        data.push(cpu.next_instruction().unwrap());

        u16::from_ne_bytes(data[0..2].try_into().unwrap())
    } else {
        u8::from_ne_bytes(data[0..1].try_into().unwrap())
            .try_into()
            .unwrap()
    };

    format!("mov [{address}], ax");

    Instruction {
        str_rep: format!("mov [{address}], ax"),
        op: Operation::MoveAccumulatorToMemory,
        operands: Operands {
            left: Operand::Address(Address::MemoryAddress(MemoryAddress {
                address: address as usize,
            })),
            right: Operand::Register(Register::Ax),
        },
    }
}

fn arithmatic_immediate_with_accumulator(byte: u8, cpu: &mut Cpu) -> Instruction {
    let mut data: Vec<u8> = Vec::new();
    let wide = byte & 1 == 1;

    let op = get_arithmatic_op(byte >> 3 & 7);

    data.push(cpu.next_instruction().unwrap());

    let immediate: i16 = if wide {
        data.push(cpu.next_instruction().unwrap());

        i16::from_ne_bytes(data[0..2].try_into().unwrap())
    } else {
        i8::from_ne_bytes(data[0..1].try_into().unwrap())
            .try_into()
            .unwrap()
    };

    let reg = if wide { "ax" } else { "al" };

    // format!("{op} {reg}, {immediate}");
    Instruction {
        str_rep: format!("{op} {reg}, {immediate}"),
        op: Operation::ArithmaticImmediateWithAccumulator,
        operands: Operands {
            left: Operand::Register(if wide { Register::Ax } else { Register::Al }),
            right: Operand::ImmediateWord(immediate as u16),
        },
    }
}

fn mov_memory_to_accumulator(byte: u8, cpu: &mut Cpu) -> Instruction {
    let mut data: Vec<u8> = Vec::new();
    let wide = byte & 1 == 1;

    data.push(cpu.next_instruction().unwrap());

    let address: u16 = if wide {
        data.push(cpu.next_instruction().unwrap());

        u16::from_ne_bytes(data[0..2].try_into().unwrap())
    } else {
        u8::from_ne_bytes(data[0..1].try_into().unwrap())
            .try_into()
            .unwrap()
    };

    //format!("mov ax, [{address}]")
    Instruction {
        str_rep: format!("mov ax, [{address}]"),
        op: Operation::MoveMemoryToAccumulator,
        operands: Operands {
            left: Operand::Address(Address::MemoryAddress(MemoryAddress {
                address: address as usize,
            })),
            right: Operand::Register(Register::Ax),
        },
    }
}

fn mov_immediate_to_reg(byte: u8, cpu: &mut Cpu) -> Instruction {
    let mut data: Vec<u8> = Vec::new();
    let wide = (byte >> 3) & 1 == 1;

    let reg = get_register_encoding(byte & 7, wide);
    //println!("; MOV immediate to register: {reg}, wide: {w}");
    data.push(cpu.next_instruction().unwrap());

    let immediate: i16 = if wide {
        data.push(cpu.next_instruction().unwrap());
        let len = data.len();

        i16::from_ne_bytes(data[len - 2..len].try_into().unwrap())
    } else {
        let len = data.len();
        i8::from_ne_bytes(data[len - 1..len].try_into().unwrap())
            .try_into()
            .unwrap()
    };

    Instruction {
        op: Operation::MovImmediateToRegister,
        operands: Operands {
            left: Operand::Register(Register::from_str(reg)),
            right: if wide {
                Operand::ImmediateWord(immediate as u16)
            } else {
                Operand::ImmediateByte(immediate as u8)
            },
        },
        str_rep: format!("mov {reg}, {immediate}"),
    }
}

fn get_effective_address_formula(data: &mut Vec<u8>, wide: bool, cpu: &mut Cpu) -> Address {
    let mod_field = (data[0] & 0b1100_0000).rotate_left(2);
    let rm = data[0] & 0b0000_0111;
    match mod_field {
        0b11 => Address::Register(Register::from_str(get_register_encoding(rm, wide))),
        _ => {
            let mut address: i16 = 0;
            let effective_address_formula =
                get_effective_address(rm.try_into().unwrap()).to_string();
            //println!("; effective_address_formula: {effective_address_formula}");
            if mod_field != 0 || rm == 6 {
                data.push(cpu.next_instruction().unwrap());

                let mut effective_address_word = false;
                if mod_field != 1 {
                    effective_address_word = true;
                    data.push(cpu.next_instruction().unwrap());
                }

                if effective_address_word {
                    address = i16::from_ne_bytes(data[1..3].try_into().unwrap());
                } else {
                    address = i8::from_ne_bytes(data[1..2].try_into().unwrap())
                        .try_into()
                        .unwrap();
                }
            }
            if mod_field == 0 && rm == 6 {
                Address::MemoryAddress(MemoryAddress {
                    address: address.try_into().unwrap(),
                })
            } else if address != 0 {
                Address::EffectiveFormula(EffectiveAddress::from_str(
                    effective_address_formula,
                    address.try_into().unwrap(),
                ))
            } else {
                Address::EffectiveFormula(EffectiveAddress::from_str(effective_address_formula, 0))
            }
        }
    }
}
