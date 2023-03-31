use std::env;
use std::fs::File;
use std::io;
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
}

struct Cpu {
    memory: Memory,
    registers: Registers,
    last_instruction: usize,
}

#[derive(Copy, Clone, Debug)]
struct Registers {
    general: [u16; 8], // 8 word sized registers
    segment: [u16; 4], // 4 word sized registers
    instruction_pointer: u16,
}

impl Registers {
    pub fn new() -> Registers {
        let general_registers = [0u16; 8];

        let segment_registers = [0u16; 4];

        Registers {
            general: general_registers,
            segment: segment_registers,
            instruction_pointer: 0,
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

    pub fn set_register(&mut self, register: Register, value: RegisterValue) {
        match (register, value) {
            (Register::Ax, RegisterValue::Word(value)) => self.general[0] = value,
            (Register::Bx, RegisterValue::Word(value)) => self.general[1] = value,
            (Register::Cx, RegisterValue::Word(value)) => self.general[2] = value,
            (Register::Dx, RegisterValue::Word(value)) => self.general[3] = value,
            (Register::Ah, RegisterValue::Byte(value)) => {
                Registers::set_high_byte(&mut self.general[0], value)
            }
            (Register::Bh, RegisterValue::Byte(value)) => {
                Registers::set_high_byte(&mut self.general[1], value)
            }
            (Register::Ch, RegisterValue::Byte(value)) => {
                Registers::set_high_byte(&mut self.general[2], value)
            }
            (Register::Dh, RegisterValue::Byte(value)) => {
                Registers::set_high_byte(&mut self.general[3], value)
            }
            (Register::Al, RegisterValue::Byte(value)) => {
                Registers::set_low_byte(&mut self.general[0], value)
            }
            (Register::Bl, RegisterValue::Byte(value)) => {
                Registers::set_low_byte(&mut self.general[1], value)
            }
            (Register::Cl, RegisterValue::Byte(value)) => {
                Registers::set_low_byte(&mut self.general[2], value)
            }
            (Register::Dl, RegisterValue::Byte(value)) => {
                Registers::set_low_byte(&mut self.general[3], value)
            }
            (Register::Sp, RegisterValue::Word(value)) => self.general[0] = value,
            (Register::Bp, RegisterValue::Word(value)) => self.general[0] = value,
            (Register::Si, RegisterValue::Word(value)) => self.general[0] = value,
            (Register::Di, RegisterValue::Word(value)) => self.general[0] = value,
            (Register::Cs, RegisterValue::Word(value)) => self.general[0] = value,
            (Register::Ds, RegisterValue::Word(value)) => self.general[0] = value,
            (Register::Ss, RegisterValue::Word(value)) => self.general[0] = value,
            (Register::Es, RegisterValue::Word(value)) => self.general[0] = value,
            (Register::Ip, RegisterValue::Word(value)) => self.general[0] = value,
            (register, value) => panic!("Register {register:?} can't take a {value:?}"),
        }
    }

    pub fn get_register(&self, register: Register) -> RegisterValue {
        match register {
            Register::Ax => RegisterValue::Word(self.general[0]),
            Register::Bx => RegisterValue::Word(self.general[1]),
            Register::Cx => RegisterValue::Word(self.general[2]),
            Register::Dx => RegisterValue::Word(self.general[3]),
            Register::Ah => RegisterValue::Byte(self.general[0].to_be_bytes()[0]),
            Register::Bh => RegisterValue::Byte(self.general[1].to_be_bytes()[0]),
            Register::Ch => RegisterValue::Byte(self.general[2].to_be_bytes()[0]),
            Register::Dh => RegisterValue::Byte(self.general[3].to_be_bytes()[0]),
            Register::Al => RegisterValue::Byte(self.general[0].to_be_bytes()[1]),
            Register::Bl => RegisterValue::Byte(self.general[1].to_be_bytes()[1]),
            Register::Cl => RegisterValue::Byte(self.general[2].to_be_bytes()[1]),
            Register::Dl => RegisterValue::Byte(self.general[3].to_be_bytes()[1]),
            Register::Sp => RegisterValue::Word(self.general[4]),
            Register::Bp => RegisterValue::Word(self.general[5]),
            Register::Si => RegisterValue::Word(self.general[6]),
            Register::Di => RegisterValue::Word(self.general[7]),
            Register::Cs => RegisterValue::Word(self.segment[0]),
            Register::Ds => RegisterValue::Word(self.segment[1]),
            Register::Ss => RegisterValue::Word(self.segment[2]),
            Register::Es => RegisterValue::Word(self.segment[3]),
            Register::Ip => RegisterValue::Word(self.instruction_pointer),
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

#[derive(Debug, PartialEq)]
enum RegisterValue {
    Byte(u8),
    Word(u16),
}

impl std::fmt::Display for RegisterValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            RegisterValue::Byte(value) => write!(f, "{:#x}", value),
            RegisterValue::Word(value) => write!(f, "{:#x}", value),
        }
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
        if let RegisterValue::Word(ip) = self.registers.get_register(Register::Ip) {
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

    pub fn execute(&mut self, instruction: &Instruction) {
        //print_comment(format!("Going to execute instruction: {instruction:?}"));
        print!("{}", instruction.str_rep);
        let pre_op_registers = self.registers.clone();
        match &instruction.op {
            Operation::MovImmediateToRegister => {
                match (&instruction.operands.left, &instruction.operands.right) {
                    (Operand::Register(register), Operand::ImmediateWord(value)) => {
                        self.registers
                            .set_register(*register, RegisterValue::Word(*value));
                    }
                    (Operand::Register(register), Operand::ImmediateByte(value)) => {
                        self.registers
                            .set_register(*register, RegisterValue::Byte(*value));
                    }
                    (left, right) => {
                        panic!(
                            "{:?} not defined for operands: ({left:?}, {right:?})",
                            instruction.op
                        )
                    }
                }
            }
        }
        let post_op_registers = self.registers.clone();
        register_changed(Register::Ax, pre_op_registers, post_op_registers);
        register_changed(Register::Bx, pre_op_registers, post_op_registers);
        register_changed(Register::Cx, pre_op_registers, post_op_registers);
        register_changed(Register::Dx, pre_op_registers, post_op_registers);
        register_changed(Register::Sp, pre_op_registers, post_op_registers);
        register_changed(Register::Bp, pre_op_registers, post_op_registers);
        register_changed(Register::Si, pre_op_registers, post_op_registers);
        register_changed(Register::Di, pre_op_registers, post_op_registers);
        register_changed(Register::Cs, pre_op_registers, post_op_registers);
        register_changed(Register::Ds, pre_op_registers, post_op_registers);
        register_changed(Register::Ss, pre_op_registers, post_op_registers);
        register_changed(Register::Es, pre_op_registers, post_op_registers);
        register_changed(Register::Ip, pre_op_registers, post_op_registers);
    }
}

fn register_changed(register: Register, pre: Registers, post: Registers) {
    match register {
        Register::Ax => {
            let pre_h = pre.get_register(Register::Ah);
            let post_h = post.get_register(Register::Ah);
            let pre_l = pre.get_register(Register::Al);
            let post_l = post.get_register(Register::Al);
            if pre_h != post_h && pre_l != post_l {
                println!(
                    " ; {}:{}->{}",
                    Register::Ax,
                    pre.get_register(Register::Ax),
                    post.get_register(Register::Ax)
                );
            } else if pre_h != post_h {
                println!(" ; {}:{}->{}", Register::Ah, pre_h, post_h);
            } else if pre_l != post_l {
                println!(" ; {}:{}->{}", Register::Al, pre_l, post_l);
            }
        }
        Register::Bx => {
            let pre_h = pre.get_register(Register::Bh);
            let post_h = post.get_register(Register::Bh);
            let pre_l = pre.get_register(Register::Bl);
            let post_l = post.get_register(Register::Bl);
            if pre_h != post_h && pre_l != post_l {
                println!(
                    " ; {}:{}->{}",
                    Register::Bx,
                    pre.get_register(Register::Bx),
                    post.get_register(Register::Bx)
                );
            } else if pre_h != post_h {
                println!(" ; {}:{}->{}", Register::Bh, pre_h, post_h);
            } else if pre_l != post_l {
                println!(" ; {}:{}->{}", Register::Bl, pre_l, post_l);
            }
        }
        Register::Cx => {
            let pre_h = pre.get_register(Register::Ch);
            let post_h = post.get_register(Register::Ch);
            let pre_l = pre.get_register(Register::Cl);
            let post_l = post.get_register(Register::Cl);
            if pre_h != post_h && pre_l != post_l {
                println!(
                    " ; {}:{}->{}",
                    Register::Cx,
                    pre.get_register(Register::Cx),
                    post.get_register(Register::Cx)
                );
            } else if pre_h != post_h {
                println!(" ; {}:{}->{}", Register::Ch, pre_h, post_h);
            } else if pre_l != post_l {
                println!(" ; {}:{}->{}", Register::Cl, pre_l, post_l);
            }
        }
        Register::Dx => {
            let pre_h = pre.get_register(Register::Dh);
            let post_h = post.get_register(Register::Dh);
            let pre_l = pre.get_register(Register::Dl);
            let post_l = post.get_register(Register::Dl);
            if pre_h != post_h && pre_l != post_l {
                println!(
                    " ; {}:{}->{}",
                    Register::Dx,
                    pre.get_register(Register::Dx),
                    post.get_register(Register::Dx)
                );
            } else if pre_h != post_h {
                println!(" ; {}:{}->{}", Register::Dh, pre_h, post_h);
            } else if pre_l != post_l {
                println!(" ; {}:{}->{}", Register::Dl, pre_l, post_l);
            }
        }
        register => {
            let pre = pre.get_register(register);
            let post = post.get_register(register);
            if pre != post {
                println!(" ; {}:{}->{}", register, pre, post);
            }
        }
    }
}

#[derive(Debug)]
struct Instruction {
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
}

#[derive(Debug)]
enum Operand {
    Register(Register),
    ImmediateByte(u8),
    ImmediateWord(u16),
}

const OPCODES: [(u8, u8, &str); 12] = [
    (0b1000_1000, 0b1111_1100, "mov-reg_mem-to_from-reg"),
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

const EFFECTIVE_ADDRESS: [&str; 8] = [
    "bx + si", "bx + di", "bp + si", "bp + di", "si", "di", "bp", "bx",
];

fn get_effective_address(rm: usize) -> &'static str {
    EFFECTIVE_ADDRESS[rm]
}

/// Prints a `comment` to our decoded assembly
fn print_comment(comment: String) {
    println!("; {comment}");
}

fn main() -> io::Result<()> {
    let mut args = env::args();
    args.next();

    let filename = args.next().unwrap();
    println!("; {filename}");

    let mut f = File::open(filename)?;

    let mut cpu = Cpu::new();

    let read_len = cpu.read_file_into_memory(&mut f, 0)?;

    println!("; Instruction stream is {} bytes long.", read_len);

    // Tell NASM to encode our ASM using basic 16bit instructions
    // so we can easily re-encode our decode to test.
    println!("\nbits 16\n");

    while let Some(byte) = cpu.next_instruction() {
        let instruction = get_instruction(byte).unwrap();

        let assembly = match instruction {
            "mov-reg_mem-to_from-reg" => mov_reg_mem_to_from_reg(byte, &mut cpu),
            "mov-immediate-to-reg" => {
                let instruction = mov_immediate_to_reg(byte, &mut cpu);
                cpu.execute(&instruction);
                "".to_string()
            }
            "mov-immediate-to-reg_mem" => mov_immediate_to_reg_mem(byte, &mut cpu),
            "mov-memory-to-accumulator" => mov_memory_to_accumulator(byte, &mut cpu),
            "mov-accumulator-to-memory" => mov_accumulator_to_memory(byte, &mut cpu),
            "arithmatic-reg_mem-and-reg-to-either" => {
                arithmatic_reg_mem_and_reg_to_either(byte, &mut cpu)
            }
            "arithmatic-immediate-to-reg_mem" => arithmatic_immediate_to_reg_mem(byte, &mut cpu),
            "arithmatic-immediate-with-accumulator" => {
                arithmatic_immediate_with_accumulator(byte, &mut cpu)
            }
            "jump" => jump(byte, &mut cpu),
            "loop" => r#loop(byte, &mut cpu),
            &_ => todo!("Not yet implemented {instruction}"),
        };

        println!("{assembly}");
    }

    Ok(())
}

const LOOP_OPS: [&str; 4] = ["loopnz", "loopz", "loop", "jcxz"];

const JUMP_OPS: [&str; 16] = [
    "jo", "jno", "jb", "jnb", "je", "jne", "jbe", "jnbe", "js", "jns", "jp", "jnp", "jl", "jnl",
    "jle", "jnle",
];

fn r#loop(byte: u8, cpu: &mut Cpu) -> String {
    let mut data: Vec<u8> = Vec::new();

    data.push(cpu.next_instruction().unwrap());

    let loop_op = LOOP_OPS[<u8 as TryInto<usize>>::try_into(byte & 3).unwrap()];

    let ip_inc8 = i8::from_ne_bytes(data[0..1].try_into().unwrap()) + 2;

    let inc_ip = if ip_inc8 > 0 {
        format!("$+{}+0", ip_inc8)
    } else if ip_inc8 == 0 {
        format!("$+0")
    } else {
        format!("${}+0", ip_inc8)
    };

    format!("{loop_op} {inc_ip}")
}

fn jump(byte: u8, cpu: &mut Cpu) -> String {
    let mut data: Vec<u8> = Vec::new();

    data.push(cpu.next_instruction().unwrap());

    let jump_op = JUMP_OPS[<u8 as TryInto<usize>>::try_into(byte & 15).unwrap()];

    let ip_inc8 = i8::from_ne_bytes(data[0..1].try_into().unwrap()) + 2;

    let inc_ip = if ip_inc8 > 0 {
        format!("$+{}+0", ip_inc8)
    } else if ip_inc8 == 0 {
        format!("$+0")
    } else {
        format!("${}+0", ip_inc8)
    };

    format!("{jump_op} {inc_ip}")
}

const ARITHMATIC_OPS: [&str; 8] = ["add", "or", "adc", "sbb", "and", "sub", "xor", "cmp"];

fn get_arithmatic_op(byte: u8) -> &'static str {
    let index: usize = byte.try_into().unwrap();
    ARITHMATIC_OPS[index]
}

fn arithmatic_reg_mem_and_reg_to_either(byte: u8, cpu: &mut Cpu) -> String {
    let mut data: Vec<u8> = Vec::new();
    let reg_is_destination = byte & 0x2 == 0x2;
    let wide = byte & 0x1 == 0x1;
    //print_cmment("REG is destination: {reg_is_destination}, Operates on word: {w}");

    let op = get_arithmatic_op(byte >> 3 & 7);

    data.push(cpu.next_instruction().unwrap());
    let reg = get_register_encoding((data[0] & 0b0011_1000).rotate_right(3), wide);
    let effective_address_formula = get_effective_address_formula(&mut data, wide, cpu);
    if reg_is_destination {
        format!("{op} {reg}, {effective_address_formula}")
    } else {
        format!("{op} {effective_address_formula}, {reg}")
    }
}

fn mov_reg_mem_to_from_reg(byte: u8, cpu: &mut Cpu) -> String {
    let mut data: Vec<u8> = Vec::new();
    let reg_is_destination = byte & 0x2 == 0x2;
    let wide = byte & 0x1 == 0x1;
    print_comment(format!(
        "REG is destination: {reg_is_destination}, Operates on word: {wide}"
    ));

    data.push(cpu.next_instruction().unwrap());
    let reg = get_register_encoding((data[0] & 0b0011_1000).rotate_right(3), wide);
    let effective_address_formula = get_effective_address_formula(&mut data, wide, cpu);
    if reg_is_destination {
        format!("mov {reg}, {effective_address_formula}")
    } else {
        format!("mov {effective_address_formula}, {reg}")
    }
}

fn arithmatic_immediate_to_reg_mem(byte: u8, cpu: &mut Cpu) -> String {
    let mut data: Vec<u8> = Vec::new();
    let wide = byte & 1 == 1;
    let signed_extension = byte & 2 == 2;

    data.push(cpu.next_instruction().unwrap());

    let op = if byte >> 3 & 7 == 6 {
        "xor"
    } else {
        get_arithmatic_op(data[0] >> 3 & 7)
    };

    let effective_address_formula = get_effective_address_formula(&mut data, wide, cpu);

    print_comment(format!(
        "signed_extension: {signed_extension}, wide: {wide}"
    ));
    data.push(cpu.next_instruction().unwrap());
    let immediate: String = if wide {
        if signed_extension {
            data.push(0);
        } else {
            data.push(cpu.next_instruction().unwrap());
        }

        let len = data.len();
        format!(
            "{}",
            i16::from_ne_bytes(data[len - 2..len].try_into().unwrap())
        )
    } else {
        let len = data.len();
        format!(
            "{}",
            i8::from_ne_bytes(data[len - 1..len].try_into().unwrap())
        )
    };

    let width_specifier = if effective_address_formula.contains("[") {
        if wide {
            "word "
        } else {
            "byte "
        }
    } else {
        ""
    };

    format!("{op} {width_specifier}{effective_address_formula}, {immediate}")
}

fn mov_immediate_to_reg_mem(byte: u8, cpu: &mut Cpu) -> String {
    let mut data: Vec<u8> = Vec::new();
    let wide = byte & 1 == 1;
    //println!("; MOV immediate to register: {reg}, wide: {w}");

    data.push(cpu.next_instruction().unwrap());
    let effective_address_formula = get_effective_address_formula(&mut data, wide, cpu);

    data.push(cpu.next_instruction().unwrap());
    let immediate: String = if wide {
        data.push(cpu.next_instruction().unwrap());

        let len = data.len();
        format!(
            "word {}",
            i16::from_ne_bytes(data[len - 2..len].try_into().unwrap())
        )
    } else {
        let len = data.len();
        format!(
            "byte {}",
            i8::from_ne_bytes(data[len - 1..len].try_into().unwrap())
        )
    };

    format!("mov {effective_address_formula}, {immediate}")
}

fn mov_accumulator_to_memory(byte: u8, cpu: &mut Cpu) -> String {
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

    format!("mov [{address}], ax")
}

fn arithmatic_immediate_with_accumulator(byte: u8, cpu: &mut Cpu) -> String {
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

    format!("{op} {reg}, {immediate}")
}

fn mov_memory_to_accumulator(byte: u8, cpu: &mut Cpu) -> String {
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

    format!("mov ax, [{address}]")
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

fn get_effective_address_formula(data: &mut Vec<u8>, wide: bool, cpu: &mut Cpu) -> String {
    let mod_field = (data[0] & 0b1100_0000).rotate_left(2);
    let rm = data[0] & 0b0000_0111;
    match mod_field {
        0b11 => get_register_encoding(rm, wide).to_string(),
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
                format!("[{address}]")
            } else if address != 0 {
                let sign = if address > 0 { "+" } else { "-" };
                format!("[{effective_address_formula} {sign} {}]", address.abs())
            } else {
                format!("[{effective_address_formula}]")
            }
        }
    }
}
