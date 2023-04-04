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

    pub fn get_address_u8(&self, address: usize) -> u8 {
        self.buffer[address]
    }

    pub fn get_address_u16(&self, address: usize) -> u16 {
        let low = self.buffer[address];
        let high = self.buffer[address + 1];
        u16::from_le_bytes([low, high])
    }

    pub fn set_address_u8(&mut self, address: usize, value: u8) {
        self.buffer[address] = value
    }

    pub fn set_address_u16(&mut self, address: usize, value: u16) {
        let bytes = value.to_le_bytes();
        self.buffer[address] = bytes[0];
        self.buffer[address + 1] = bytes[1];
    }
}

// TODO: Change this to a struct with an EffectiveAddressType field and a displacement field
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
            Self::BxSi(displacement) => write!(f, "[bx + si {:+}]", *displacement as i16),
            Self::BxDi(displacement) => write!(f, "[bx + di {:+}]", *displacement as i16),
            Self::BpSi(displacement) => write!(f, "[bp + si {:+}]", *displacement as i16),
            Self::BpDi(displacement) => write!(f, "[bp + di {:+}]", *displacement as i16),
            Self::Si(displacement) => write!(f, "[si {:+}]", *displacement as i16),
            Self::Di(displacement) => write!(f, "[di {:+}]", *displacement as i16),
            Self::Bp(displacement) => write!(f, "[bp {:+}]", *displacement as i16),
            Self::Bx(displacement) => write!(f, "[bx {:+}]", *displacement as i16),
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

    pub fn jump_to_offset(&mut self, offset: i8) {
        self.instruction_pointer = (self.instruction_pointer as i16 + offset as i16) as u16;
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

    pub fn set_register_u16(&mut self, register: Register, value: u16) {
        match register {
            Register::Ax => self.general[0] = value,
            Register::Bx => self.general[1] = value,
            Register::Cx => self.general[2] = value,
            Register::Dx => self.general[3] = value,
            Register::Si => self.general[4] = value,
            Register::Di => self.general[5] = value,
            Register::Sp => self.general[6] = value,
            Register::Bp => self.general[7] = value,
            Register::Cs => self.segment[0] = value,
            Register::Ds => self.segment[1] = value,
            Register::Es => self.segment[2] = value,
            Register::Ss => self.segment[3] = value,
            Register::Ip => self.instruction_pointer = value,
            _ => panic!("Invalid register"),
        }
    }

    pub fn set_register_u8(&mut self, register: Register, value: u8) {
        match register {
            Register::Al => self.general[0] = (self.general[0] & 0xFF00) | value as u16,
            Register::Bl => self.general[1] = (self.general[1] & 0xFF00) | value as u16,
            Register::Cl => self.general[2] = (self.general[2] & 0xFF00) | value as u16,
            Register::Dl => self.general[3] = (self.general[3] & 0xFF00) | value as u16,
            Register::Ah => self.general[0] = (self.general[0] & 0x00FF) | ((value as u16) << 8),
            Register::Bh => self.general[1] = (self.general[1] & 0x00FF) | ((value as u16) << 8),
            Register::Ch => self.general[2] = (self.general[2] & 0x00FF) | ((value as u16) << 8),
            Register::Dh => self.general[3] = (self.general[3] & 0x00FF) | ((value as u16) << 8),
            _ => panic!("Invalid register"),
        }
    }

    pub fn get_register_u16(&self, register: Register) -> u16 {
        match register {
            Register::Ax => self.general[0],
            Register::Bx => self.general[1],
            Register::Cx => self.general[2],
            Register::Dx => self.general[3],
            Register::Si => self.general[4],
            Register::Di => self.general[5],
            Register::Sp => self.general[6],
            Register::Bp => self.general[7],
            Register::Cs => self.segment[0],
            Register::Ds => self.segment[1],
            Register::Es => self.segment[2],
            Register::Ss => self.segment[3],
            Register::Ip => self.instruction_pointer,
            _ => panic!("Invalid register"),
        }
    }

    pub fn get_register_u8(&self, register: Register) -> u8 {
        match register {
            Register::Al => (self.general[0] & 0x00FF) as u8,
            Register::Bl => (self.general[1] & 0x00FF) as u8,
            Register::Cl => (self.general[2] & 0x00FF) as u8,
            Register::Dl => (self.general[3] & 0x00FF) as u8,
            Register::Ah => (self.general[0] >> 8) as u8,
            Register::Bh => (self.general[1] >> 8) as u8,
            Register::Ch => (self.general[2] >> 8) as u8,
            Register::Dh => (self.general[3] >> 8) as u8,
            _ => panic!("Invalid register"),
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
        let ip = self.registers.get_register_u16(Register::Ip);
        let next_instruction = self
            .memory
            .read_u8(self.registers.code_segment() * 64 * 1024 + ip);
        self.registers.increment_instruction_pointer();
        if self.last_instruction < self.registers.instruction_pointer.try_into().unwrap() {
            None
        } else {
            Some(next_instruction)
        }
    }

    pub fn run(&mut self, execute: bool) {
        let mut instruction_count = 0;
        let mut pre_op_registers = self.registers;
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

            if execute {
                self.execute(&instruction);
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

                pre_op_registers = self.registers;

                instruction_count += 1;

                if instruction_count > 100 {
                    panic!("Too many instructions.");
                }
            } else {
                println!("{}", instruction.str_rep);
            }
        }
        if execute {
            println!("\nFinal registers:");
            self.print_registers(pre_op_registers);
        }
    }

    pub fn execute(&mut self, instruction: &Instruction) {
        //print_comment(format!("Going to execute instruction: {instruction:?}"));
        print!("{}", instruction.str_rep);
        match &instruction.op {
            Operation::MoveMemoryToAccumulator(wide)
            | Operation::MoveAccumulatorToMemory(wide)
            | Operation::MoveImmediateToRegister(wide)
            | Operation::MoveRegisterMemoryToFromRegister(wide)
            | Operation::MoveImmediateToRegisterOrMemory(wide) => {
                self.execute_mov_instruction(instruction, *wide);
            }
            Operation::Jump(jump_type) => match jump_type {
                JumpType::Jne => match &instruction.operands.left {
                    Operand::SignedInstructionPointerIncrement(imm) => {
                        if *imm < 0 {
                            if !self.registers.get_flag(Flag::Zf) {
                                self.registers.set_register_u16(
                                    Register::Ip,
                                    self.registers.get_register_u16(Register::Ip)
                                        - imm.unsigned_abs() as u16,
                                );
                            }
                        } else if !self.registers.get_flag(Flag::Zf) {
                            self.registers.set_register_u16(
                                Register::Ip,
                                self.registers.get_register_u16(Register::Ip) + *imm as u16,
                            );
                        }
                    }
                    _ => panic!(
                        "Jne not defined for operand: {:?}",
                        instruction.operands.left
                    ),
                },
                JumpType::Je => match &instruction.operands.left {
                    Operand::SignedInstructionPointerIncrement(imm) => {
                        if self.registers.get_flag(Flag::Zf) {
                            self.registers.jump_to_offset(*imm);
                        }
                    }
                    _ => panic!(
                        "Je not defined for operand: {:?}",
                        instruction.operands.left
                    ),
                },
                JumpType::Jb => match &instruction.operands.left {
                    Operand::SignedInstructionPointerIncrement(imm) => {
                        if self.registers.get_flag(Flag::Cf) {
                            self.registers.jump_to_offset(*imm);
                        }
                    }
                    _ => panic!(
                        "Jb not defined for operand: {:?}",
                        instruction.operands.left
                    ),
                },
                JumpType::Jp => match &instruction.operands.left {
                    Operand::SignedInstructionPointerIncrement(imm) => {
                        if self.registers.get_flag(Flag::Pf) {
                            self.registers.jump_to_offset(*imm);
                        }
                    }
                    _ => panic!(
                        "Jp not defined for operand: {:?}",
                        instruction.operands.left
                    ),
                },
                _ => todo!(),
            },
            Operation::Loop(loop_type) => match loop_type {
                LoopType::Loopnz => match &instruction.operands.left {
                    Operand::SignedInstructionPointerIncrement(imm) => {
                        if !self.registers.get_flag(Flag::Zf) {
                            self.registers.set_register_u16(
                                Register::Cx,
                                self.registers.get_register_u16(Register::Cx) - 1,
                            );
                            if self.registers.get_register_u16(Register::Cx) != 0 {
                                self.registers.jump_to_offset(*imm);
                            }
                        }
                    }
                    _ => panic!("Invalid operands for loopnz: {:?}", instruction.operands),
                },
                _ => todo!(),
            },
            Operation::ArithmaticRegisterOrMemoryAndRegisterToEither(arithmatic_type, wide)
            | Operation::ArithmaticImmediateToRegisterOrMemory(arithmatic_type, wide)
            | Operation::ArithmaticImmediateWithAccumulator(arithmatic_type, wide) => {
                self.execute_arithmatic_instruction(instruction, *arithmatic_type, *wide);
            }
        }
    }

    fn execute_mov_instruction(&mut self, instruction: &Instruction, wide: bool) {
        let source_value = match &instruction.operands.right {
            Operand::Register(register) => {
                if wide {
                    self.registers.get_register_u16(*register)
                } else {
                    self.registers.get_register_u8(*register) as u16
                }
            }
            Operand::EffectiveAddress(effective_address) => {
                if wide {
                    self.memory
                        .get_address_u16(self.calculate_address(*effective_address))
                } else {
                    self.memory
                        .get_address_u8(self.calculate_address(*effective_address))
                        as u16
                }
            }
            Operand::Address(address) => {
                if wide {
                    self.memory.get_address_u16(*address)
                } else {
                    self.memory.get_address_u8(*address) as u16
                }
            }
            Operand::ImmediateWord(imm) => *imm,
            Operand::ImmediateByte(imm) => *imm as u16,
            _ => unreachable!(),
        };

        match &instruction.operands.left {
            Operand::Register(register) => {
                if wide {
                    self.registers.set_register_u16(*register, source_value);
                } else {
                    self.registers
                        .set_register_u8(*register, source_value as u8);
                }
            }
            Operand::EffectiveAddress(effective_address) => {
                if wide {
                    self.memory
                        .set_address_u16(self.calculate_address(*effective_address), source_value);
                } else {
                    self.memory.set_address_u8(
                        self.calculate_address(*effective_address),
                        source_value as u8,
                    );
                }
            }
            Operand::Address(address) => {
                if wide {
                    self.memory.set_address_u16(*address, source_value);
                } else {
                    self.memory.set_address_u8(*address, source_value as u8);
                }
            }
            _ => unreachable!(),
        }
    }
    fn execute_arithmatic_instruction(
        &mut self,
        instruction: &Instruction,
        arithmatic_type: ArithmaticType,
        wide: bool,
    ) {
        let dest_value = match &instruction.operands.left {
            Operand::Register(register) => {
                if wide {
                    self.registers.get_register_u16(*register)
                } else {
                    self.registers.get_register_u8(*register) as u16
                }
            }
            Operand::Address(address) => {
                if wide {
                    self.memory.get_address_u16(*address)
                } else {
                    self.memory.get_address_u8(*address) as u16
                }
            }
            Operand::EffectiveAddress(effective_address) => {
                if wide {
                    self.memory
                        .get_address_u16(self.calculate_address(*effective_address))
                } else {
                    self.memory
                        .get_address_u8(self.calculate_address(*effective_address))
                        as u16
                }
            }
            _ => unreachable!(),
        };

        let source_value = match &instruction.operands.right {
            Operand::Register(register) => {
                if wide {
                    self.registers.get_register_u16(*register)
                } else {
                    self.registers.get_register_u8(*register) as u16
                }
            }
            Operand::Address(address) => {
                if wide {
                    self.memory.get_address_u16(*address)
                } else {
                    self.memory.get_address_u8(*address) as u16
                }
            }
            Operand::EffectiveAddress(effective_address) => {
                if wide {
                    self.memory
                        .get_address_u16(self.calculate_address(*effective_address))
                } else {
                    self.memory
                        .get_address_u8(self.calculate_address(*effective_address))
                        as u16
                }
            }
            Operand::ImmediateWord(imm) => *imm,
            _ => unreachable!(),
        };

        let (result, update_dest) = match arithmatic_type {
            ArithmaticType::Add => (dest_value.wrapping_add(source_value), true),
            ArithmaticType::Adc => {
                todo!()
            }
            ArithmaticType::Or => (dest_value | source_value, true),
            ArithmaticType::Sbb => {
                todo!()
            }
            ArithmaticType::And => (dest_value & source_value, true),
            ArithmaticType::Sub => (dest_value.wrapping_sub(source_value), true),
            ArithmaticType::Xor => (dest_value ^ source_value, true),
            ArithmaticType::Cmp => (dest_value.wrapping_sub(source_value), false),
        };

        if update_dest {
            match &instruction.operands.left {
                Operand::Register(register) => {
                    if wide {
                        self.registers.set_register_u16(*register, result);
                    } else {
                        self.registers.set_register_u8(*register, result as u8);
                    }
                }
                Operand::Address(address) => {
                    if wide {
                        self.memory.set_address_u16(*address, result);
                    } else {
                        self.memory.set_address_u8(*address, result as u8);
                    }
                }
                Operand::EffectiveAddress(effective_address) => {
                    if wide {
                        self.memory
                            .set_address_u16(self.calculate_address(*effective_address), result);
                    } else {
                        self.memory.set_address_u8(
                            self.calculate_address(*effective_address),
                            result as u8,
                        );
                    }
                }
                _ => unreachable!(),
            }
        }

        match arithmatic_type {
            ArithmaticType::Sub | ArithmaticType::Cmp | ArithmaticType::Add => {
                self.registers.set_flag(Flag::Zf, result == 0);
                // Set Flag::Pf if the result has even parity.
                self.registers
                    .set_flag(Flag::Pf, (result & 0x00FF).count_ones() % 2 == 0);

                // Set Flag::Sf if the most significant bit is set in result
                let significant_bit = if wide { 0x8000 } else { 0x80 };

                self.registers
                    .set_flag(Flag::Sf, result & significant_bit == significant_bit);

                // Set Flag::Of if the most significant bit was turned on
                self.registers.set_flag(
                    Flag::Of,
                    (dest_value & significant_bit == 0)
                        && (result & significant_bit == significant_bit),
                );

                // Set Flag::Cf if the dest_value is less than source_value
                self.registers.set_flag(Flag::Cf, dest_value < source_value);
            }
            _ => {}
        }
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

    fn calculate_address(&self, effective_address_formula: EffectiveAddress) -> usize {
        match effective_address_formula {
            EffectiveAddress::BxSi(displacement) => (self.registers.get_register_u16(Register::Bx)
                + self.registers.get_register_u16(Register::Si)
                + displacement)
                .into(),
            EffectiveAddress::BxDi(displacement) => (self
                .registers
                .get_register_u16(Register::Bx)
                .wrapping_add(self.registers.get_register_u16(Register::Di))
                .wrapping_add(displacement))
            .into(),
            EffectiveAddress::BpSi(displacement) => (self.registers.get_register_u16(Register::Bp)
                + self.registers.get_register_u16(Register::Si)
                + displacement)
                .into(),
            EffectiveAddress::BpDi(displacement) => (self.registers.get_register_u16(Register::Bp)
                + self.registers.get_register_u16(Register::Di)
                + displacement)
                .into(),
            EffectiveAddress::Si(displacement) => {
                (self.registers.get_register_u16(Register::Si) + displacement).into()
            }
            EffectiveAddress::Di(displacement) => {
                (self.registers.get_register_u16(Register::Di) + displacement).into()
            }
            EffectiveAddress::Bp(displacement) => {
                (self.registers.get_register_u16(Register::Bp) + displacement).into()
            }
            EffectiveAddress::Bx(displacement) => (self
                .registers
                .get_register_u16(Register::Bx)
                .wrapping_add(displacement))
            .into(),
        }
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

    fn print_registers(&self, registers: Registers) {
        for register in Self::DEBUG_REGISTERS {
            let value = registers.get_register_u16(register);
            println!("\t{register}: 0x{value:04x} ({value})",);
        }
        let flags: String = Self::DEBUG_FLAGS
            .iter()
            .filter(|f| registers.get_flag(**f))
            .map(|f| format!("{}", f))
            .collect();
        println!("\tflags: {flags}");
    }
}

fn register_changed(register: Register, pre: Registers, post: Registers) {
    let pre = pre.get_register_u16(register);
    let post = post.get_register_u16(register);
    if pre != post {
        print!("{}:0x{:04x}->0x{:04x} ", register, pre, post);
    }
}

#[derive(Debug, Clone)]
pub struct Instruction {
    op: Operation,
    operands: Operands,
    str_rep: String,
}

#[derive(Debug, Copy, Clone)]
struct Operands {
    left: Operand,
    right: Operand,
}

#[derive(Debug, Copy, Clone)]
enum Operation {
    MoveImmediateToRegister(bool),
    MoveRegisterMemoryToFromRegister(bool),
    MoveImmediateToRegisterOrMemory(bool),
    MoveMemoryToAccumulator(bool),
    MoveAccumulatorToMemory(bool),
    Loop(LoopType),
    Jump(JumpType),
    ArithmaticImmediateWithAccumulator(ArithmaticType, bool),
    ArithmaticRegisterOrMemoryAndRegisterToEither(ArithmaticType, bool),
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

impl std::fmt::Display for JumpType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            JumpType::Je => write!(f, "je"),
            JumpType::Jl => write!(f, "jl"),
            JumpType::Jle => write!(f, "jle"),
            JumpType::Jb => write!(f, "jb"),
            JumpType::Jbe => write!(f, "jbe"),
            JumpType::Jp => write!(f, "jp"),
            JumpType::Jo => write!(f, "jo"),
            JumpType::Js => write!(f, "js"),
            JumpType::Jne => write!(f, "jne"),
            JumpType::Jnl => write!(f, "jnl"),
            JumpType::Jnle => write!(f, "jnle"),
            JumpType::Jnb => write!(f, "jnb"),
            JumpType::Jnbe => write!(f, "jnbe"),
            JumpType::Jnp => write!(f, "jnp"),
            JumpType::Jno => write!(f, "jno"),
            JumpType::Jns => write!(f, "jns"),
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum LoopType {
    Loop,
    Loopz,
    Loopnz,
    Jcxz,
}

impl std::fmt::Display for LoopType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LoopType::Loop => write!(f, "loop"),
            LoopType::Loopz => write!(f, "loopz"),
            LoopType::Loopnz => write!(f, "loopnz"),
            LoopType::Jcxz => write!(f, "jcxz"),
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum Operand {
    Register(Register),
    ImmediateByte(u8),
    ImmediateWord(u16),
    Address(usize),
    EffectiveAddress(EffectiveAddress),
    SignedInstructionPointerIncrement(i8),
    None,
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Operand::Register(r) => write!(f, "{}", r),
            Operand::ImmediateByte(b) => write!(f, "0x{:02x}", b),
            Operand::ImmediateWord(w) => write!(f, "0x{:04x}", w),
            Operand::Address(a) => write!(f, "0x{:04x}", a),
            Operand::EffectiveAddress(ea) => write!(f, "{}", ea),
            Operand::SignedInstructionPointerIncrement(i) => write!(f, "0x{:02x}", i),
            Operand::None => write!(f, ""),
        }
    }
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
                                        //   (0b1111_1111, 0b1111_1111, "call-indirect"),
];

fn get_instruction(byte: u8) -> Option<&'static str> {
    for (opcode, mask, instruction) in OPCODES {
        if (byte & mask) == opcode {
            return Some(instruction);
        }
    }

    panic!("Couldn't find instruction for byte {byte:#b}");
}

const REGISTER_ENCODING: [(Register, Register); 8] = [
    (Register::Al, Register::Ax),
    (Register::Cl, Register::Cx),
    (Register::Dl, Register::Dx),
    (Register::Bl, Register::Bx),
    (Register::Ah, Register::Sp),
    (Register::Ch, Register::Bp),
    (Register::Dh, Register::Si),
    (Register::Bh, Register::Di),
];

fn get_register_encoding(reg: u8, word: bool) -> Register {
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
    LoopType::Loopnz,
    LoopType::Loopz,
    LoopType::Loop,
    LoopType::Jcxz,
];

const JUMP_OPS: [JumpType; 16] = [
    JumpType::Jo,
    JumpType::Jno,
    JumpType::Jb,
    JumpType::Jnb,
    JumpType::Je,
    JumpType::Jne,
    JumpType::Jbe,
    JumpType::Jnbe,
    JumpType::Js,
    JumpType::Jns,
    JumpType::Jp,
    JumpType::Jnp,
    JumpType::Jl,
    JumpType::Jnl,
    JumpType::Jle,
    JumpType::Jnle,
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
            left: Operand::SignedInstructionPointerIncrement(ip_inc8 - 2),
            right: Operand::None,
        },
        str_rep: format!("{loop_op} {inc_ip}"),
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
            left: Operand::SignedInstructionPointerIncrement(ip_inc8 - 2),
            right: Operand::None,
        },
        str_rep: format!("{jump_op} {inc_ip}"),
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
            str_rep: format!("{op} {reg}, {effective_address_formula}"),
            op: Operation::ArithmaticRegisterOrMemoryAndRegisterToEither(op, wide),
            operands: Operands {
                left: Operand::Register(reg),
                right: effective_address_formula,
            },
        }
    } else {
        Instruction {
            str_rep: format!("{op} {effective_address_formula}, {reg}"),
            op: Operation::ArithmaticRegisterOrMemoryAndRegisterToEither(op, wide),
            operands: Operands {
                left: effective_address_formula,
                right: Operand::Register(reg),
            },
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
            str_rep: format!("mov {segment_register}, {effective_address_formula}"),
            op: Operation::MoveRegisterMemoryToFromRegister(true),
            operands: Operands {
                left: Operand::Register(segment_register),
                right: effective_address_formula,
            },
        }
    } else {
        Instruction {
            str_rep: format!("mov {effective_address_formula}, {segment_register}"),
            op: Operation::MoveRegisterMemoryToFromRegister(true),
            operands: Operands {
                left: effective_address_formula,
                right: Operand::Register(segment_register),
            },
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
            str_rep: format!("mov {reg}, {effective_address_formula}"),
            op: Operation::MoveRegisterMemoryToFromRegister(wide),
            operands: Operands {
                left: Operand::Register(reg),
                right: effective_address_formula,
            },
        }
    } else {
        Instruction {
            str_rep: format!("mov {effective_address_formula}, {reg}"),
            op: Operation::MoveRegisterMemoryToFromRegister(wide),
            operands: Operands {
                left: effective_address_formula,
                right: Operand::Register(reg),
            },
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
            left: effective_address_formula,
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
            left: effective_address_formula,
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
        op: Operation::MoveAccumulatorToMemory(wide),
        operands: Operands {
            left: Operand::Address(address as usize),
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
        op: Operation::ArithmaticImmediateWithAccumulator(op, wide),
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
        op: Operation::MoveMemoryToAccumulator(wide),
        operands: Operands {
            left: Operand::Address(address as usize),
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
        op: Operation::MoveImmediateToRegister(wide),
        operands: Operands {
            left: Operand::Register(reg),
            right: if wide {
                Operand::ImmediateWord(immediate as u16)
            } else {
                Operand::ImmediateByte(immediate as u8)
            },
        },
        str_rep: format!("mov {reg}, {immediate}"),
    }
}

fn get_effective_address_formula(data: &mut Vec<u8>, wide: bool, cpu: &mut Cpu) -> Operand {
    let mod_field = (data[0] & 0b1100_0000).rotate_left(2);
    let rm = data[0] & 0b0000_0111;
    match mod_field {
        0b11 => Operand::Register(get_register_encoding(rm, wide)),
        _ => {
            let mut address: i16 = 0;
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
            let effective_address_formula =
                get_effective_address(rm.try_into().unwrap()).to_string();
            if mod_field == 0 && rm == 6 {
                Operand::Address(address.try_into().unwrap())
            } else if address != 0 {
                Operand::EffectiveAddress(EffectiveAddress::from_str(
                    effective_address_formula,
                    address as u16,
                ))
            } else {
                Operand::EffectiveAddress(EffectiveAddress::from_str(effective_address_formula, 0))
            }
        }
    }
}
