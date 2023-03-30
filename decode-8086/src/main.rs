use std::env;
use std::fs::File;
use std::io;
use std::io::Read;

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

fn main() -> io::Result<()> {
    let mut args = env::args();
    args.next();

    let filename = args.next().unwrap();
    println!("; {filename}");

    let mut f = File::open(filename)?;

    // The memory for our 8086 simulator
    let mut memory: Vec<u8> = Vec::new();

    let read_len = f.read_to_end(&mut memory[..])?;

    println!("; Instruction stream is {} bytes long.", read_len);

    println!("\nbits 16\n");
    let mut bytes = memory.iter();
    while let Some(byte) = bytes.next() {
        //println!("; {_i}: {byte:#b}");
        let instruction = get_instruction(*byte).unwrap();
        //println!("; Got instruction: {instruction}.");

        let assembly = match instruction {
            "mov-reg_mem-to_from-reg" => mov_reg_mem_to_from_reg(*byte, &mut bytes),
            "mov-immediate-to-reg" => mov_immediate_to_reg(*byte, &mut bytes),
            "mov-immediate-to-reg_mem" => mov_immediate_to_reg_mem(*byte, &mut bytes),
            "mov-memory-to-accumulator" => mov_memory_to_accumulator(*byte, &mut bytes),
            "mov-accumulator-to-memory" => mov_accumulator_to_memory(*byte, &mut bytes),
            "arithmatic-reg_mem-and-reg-to-either" => {
                arithmatic_reg_mem_and_reg_to_either(*byte, &mut bytes)
            }
            "arithmatic-immediate-to-reg_mem" => arithmatic_immediate_to_reg_mem(*byte, &mut bytes),
            "arithmatic-immediate-with-accumulator" => {
                arithmatic_immediate_with_accumulator(*byte, &mut bytes)
            }
            "jump" => jump(*byte, &mut bytes),
            "loop" => r#loop(*byte, &mut bytes),
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

fn r#loop(byte: u8, bytes: &mut std::slice::Iter<u8>) -> String {
    let mut data: Vec<u8> = Vec::new();

    data.push(*bytes.next().unwrap());

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

fn jump(byte: u8, bytes: &mut std::slice::Iter<u8>) -> String {
    let mut data: Vec<u8> = Vec::new();

    data.push(*bytes.next().unwrap());

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

fn arithmatic_reg_mem_and_reg_to_either(byte: u8, bytes: &mut std::slice::Iter<u8>) -> String {
    let mut data: Vec<u8> = Vec::new();
    let reg_is_destination = byte & 0x2 == 0x2;
    let wide = byte & 0x1 == 0x1;
    //println!("; REG is destination: {reg_is_destination}, Operates on word: {w}");

    let op = get_arithmatic_op(byte >> 3 & 7);

    data.push(*bytes.next().unwrap());
    let reg = get_register_encoding((data[0] & 0b0011_1000).rotate_right(3), wide);
    let effective_address_formula = get_effective_address_formula(&mut data, wide, bytes);
    if reg_is_destination {
        format!("{op} {reg}, {effective_address_formula}")
    } else {
        format!("{op} {effective_address_formula}, {reg}")
    }
}

fn mov_reg_mem_to_from_reg(byte: u8, bytes: &mut std::slice::Iter<u8>) -> String {
    let mut data: Vec<u8> = Vec::new();
    let reg_is_destination = byte & 0x2 == 0x2;
    let wide = byte & 0x1 == 0x1;
    //println!("; REG is destination: {reg_is_destination}, Operates on word: {w}");

    data.push(*bytes.next().unwrap());
    let reg = get_register_encoding((data[0] & 0b0011_1000).rotate_right(3), wide);
    let effective_address_formula = get_effective_address_formula(&mut data, wide, bytes);
    if reg_is_destination {
        format!("mov {reg}, {effective_address_formula}")
    } else {
        format!("mov {effective_address_formula}, {reg}")
    }
}

fn arithmatic_immediate_to_reg_mem(byte: u8, bytes: &mut std::slice::Iter<u8>) -> String {
    let mut data: Vec<u8> = Vec::new();
    let wide = byte & 1 == 1;
    let signed_extension = byte & 2 == 2;
    //println!("; MOV immediate to register: {reg}, wide: {w}");

    data.push(*bytes.next().unwrap());

    let op = if byte >> 3 & 7 == 6 {
        "xor"
    } else {
        get_arithmatic_op(data[0] >> 3 & 7)
    };

    let effective_address_formula = get_effective_address_formula(&mut data, wide, bytes);

    //println!("signed_extension: {signed_extension}, wide: {wide}");
    data.push(*bytes.next().unwrap());
    let immediate: String = if wide {
        if signed_extension {
            data.push(0);
        } else {
            data.push(*bytes.next().unwrap());
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

fn mov_immediate_to_reg_mem(byte: u8, bytes: &mut std::slice::Iter<u8>) -> String {
    let mut data: Vec<u8> = Vec::new();
    let wide = byte & 1 == 1;
    //println!("; MOV immediate to register: {reg}, wide: {w}");

    data.push(*bytes.next().unwrap());
    let effective_address_formula = get_effective_address_formula(&mut data, wide, bytes);

    data.push(*bytes.next().unwrap());
    let immediate: String = if wide {
        data.push(*bytes.next().unwrap());

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

fn mov_accumulator_to_memory(byte: u8, bytes: &mut std::slice::Iter<u8>) -> String {
    let mut data: Vec<u8> = Vec::new();
    let wide = byte & 1 == 1;

    data.push(*bytes.next().unwrap());

    let address: u16 = if wide {
        data.push(*bytes.next().unwrap());

        u16::from_ne_bytes(data[0..2].try_into().unwrap())
    } else {
        u8::from_ne_bytes(data[0..1].try_into().unwrap())
            .try_into()
            .unwrap()
    };

    format!("mov [{address}], ax")
}

fn arithmatic_immediate_with_accumulator(byte: u8, bytes: &mut std::slice::Iter<u8>) -> String {
    let mut data: Vec<u8> = Vec::new();
    let wide = byte & 1 == 1;

    let op = get_arithmatic_op(byte >> 3 & 7);

    data.push(*bytes.next().unwrap());

    let immediate: i16 = if wide {
        data.push(*bytes.next().unwrap());

        i16::from_ne_bytes(data[0..2].try_into().unwrap())
    } else {
        i8::from_ne_bytes(data[0..1].try_into().unwrap())
            .try_into()
            .unwrap()
    };

    let reg = if wide { "ax" } else { "al" };

    format!("{op} {reg}, {immediate}")
}

fn mov_memory_to_accumulator(byte: u8, bytes: &mut std::slice::Iter<u8>) -> String {
    let mut data: Vec<u8> = Vec::new();
    let wide = byte & 1 == 1;

    data.push(*bytes.next().unwrap());

    let address: u16 = if wide {
        data.push(*bytes.next().unwrap());

        u16::from_ne_bytes(data[0..2].try_into().unwrap())
    } else {
        u8::from_ne_bytes(data[0..1].try_into().unwrap())
            .try_into()
            .unwrap()
    };

    format!("mov ax, [{address}]")
}

fn mov_immediate_to_reg(byte: u8, bytes: &mut std::slice::Iter<u8>) -> String {
    let mut data: Vec<u8> = Vec::new();
    let wide = (byte >> 3) & 1 == 1;

    let reg = get_register_encoding(byte & 7, wide);
    //println!("; MOV immediate to register: {reg}, wide: {w}");
    data.push(*bytes.next().unwrap());

    let immediate: i16 = if wide {
        data.push(*bytes.next().unwrap());
        let len = data.len();

        i16::from_ne_bytes(data[len - 2..len].try_into().unwrap())
    } else {
        let len = data.len();
        i8::from_ne_bytes(data[len - 1..len].try_into().unwrap())
            .try_into()
            .unwrap()
    };

    format!("mov {reg}, {immediate}")
}

fn get_effective_address_formula(
    data: &mut Vec<u8>,
    wide: bool,
    bytes: &mut std::slice::Iter<u8>,
) -> String {
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
                data.push(*bytes.next().unwrap());

                let mut effective_address_word = false;
                if mod_field != 1 {
                    effective_address_word = true;
                    data.push(*bytes.next().unwrap());
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
