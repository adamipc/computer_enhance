use std::env;
use std::fs::File;
use std::io;
use std::io::Read;

const OPCODES: [(u8, u8, &str); 5] = [
    (0b100_01000, 0b1111_1100, "mov-reg_mem-to_from-reg"),
    (0b101_10000, 0b1111_0000, "mov-immediate-to-reg"),
    (0b110_00110, 0b1111_1110, "mov-immediate-to-reg_mem"),
    (0b101_00000, 0b1111_1110, "mov-memory-to-accumulator"),
    (0b101_00010, 0b1111_1110, "mov-accumulator-to-memory"),
];

fn get_instruction(byte: u8) -> Option<&'static str> {
    for (opcode, mask, instruction) in OPCODES {
        if (byte & mask) == opcode {
            return Some(instruction);
        }
    }

    panic!("Couldn't find instruction for byte {byte:#b}");
}

const REGISTER_ENCODING: [(u8, (&str, &str)); 8] = [
    (0b000, ("al", "ax")),
    (0b001, ("cl", "cx")),
    (0b010, ("dl", "dx")),
    (0b011, ("bl", "bx")),
    (0b100, ("ah", "sp")),
    (0b101, ("ch", "bp")),
    (0b110, ("dh", "si")),
    (0b111, ("bh", "di")),
];

fn get_register_encoding(byte: u8, word: bool) -> Option<&'static str> {
    REGISTER_ENCODING.iter().find_map(|(key, val)| {
        if byte == *key {
            if word {
                Some(val.1)
            } else {
                Some(val.0)
            }
        } else {
            None
        }
    })
}

const EFFECTIVE_ADDRESS: [&str; 8] = [
    "bx + si", "bx + di", "bp + si", "bp + di", "si", "di", "bp", "bx",
];

fn get_effective_address(rm: usize) -> &'static str {
    EFFECTIVE_ADDRESS[rm]
}

fn main() -> io::Result<()> {
    let mut buffer = Vec::new();

    let mut args = env::args();
    args.next();

    let filename = args.next().unwrap();
    println!("; {filename}");

    let mut f = File::open(filename)?;

    f.read_to_end(&mut buffer)?;

    println!("; Instruction stream is {} bytes long.", buffer.len());

    println!("\nbits 16\n");
    let mut bytes = buffer.iter();
    while let Some(byte) = bytes.next() {
        //println!("; {_i}: {byte:#b}");
        let instruction = get_instruction(*byte).unwrap();
        //println!("; Got instruction: {instruction}.");

        let assembly;
        match instruction {
            "mov-reg_mem-to_from-reg" => {
                (assembly, bytes) = mov_reg_mem_to_from_reg(*byte, bytes);
            }
            "mov-immediate-to-reg" => {
                (assembly, bytes) = mov_immediate_to_reg(*byte, bytes);
            }
            "mov-immediate-to-reg_mem" => {
                (assembly, bytes) = mov_immediate_to_reg_mem(*byte, bytes);
            }
            "mov-memory-to-accumulator" => {
                (assembly, bytes) = mov_memory_to_accumulator(*byte, bytes);
            }
            "mov-accumulator-to-memory" => {
                (assembly, bytes) = mov_accumulator_to_memory(*byte, bytes);
            }
            &_ => todo!("Not yet implemented {instruction}"),
        }

        println!("{assembly}");
    }

    Ok(())
}

fn mov_reg_mem_to_from_reg(
    byte: u8,
    mut bytes: std::slice::Iter<u8>,
) -> (String, std::slice::Iter<u8>) {
    let mut data: Vec<u8> = Vec::new();
    let reg_is_destination = byte & 0x2 == 0x2;
    let wide = byte & 0x1 == 0x1;
    //println!("; REG is destination: {reg_is_destination}, Operates on word: {w}");
    data.push(*bytes.next().unwrap());
    let reg = get_register_encoding((data[0] & 0b0011_1000).rotate_right(3), wide).unwrap();
    let effective_address_formula = get_effective_address_formula(&mut data, wide, &mut bytes);
    if reg_is_destination {
        (format!("mov {reg}, {effective_address_formula}"), bytes)
    } else {
        (format!("mov {effective_address_formula}, {reg}"), bytes)
    }
}

fn get_effective_address_formula(
    data: &mut Vec<u8>,
    wide: bool,
    bytes: &mut std::slice::Iter<u8>,
) -> String {
    let mod_field = (data[0] & 0b1100_0000).rotate_left(2);
    let rm = data[0] & 0b0000_0111;
    match mod_field {
        0b11 => get_register_encoding(rm, wide).unwrap().to_string(),
        _ => {
            let mut address: i16 = 0;
            let effective_address_formula =
                get_effective_address(rm.try_into().unwrap()).to_string();
            println!("; effective_address_formula: {effective_address_formula}");
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

fn mov_immediate_to_reg_mem(
    byte: u8,
    mut bytes: std::slice::Iter<u8>,
) -> (String, std::slice::Iter<u8>) {
    let mut data: Vec<u8> = Vec::new();
    let wide = byte & 1 == 1;
    //println!("; MOV immediate to register: {reg}, wide: {w}");

    data.push(*bytes.next().unwrap());
    let effective_address_formula = get_effective_address_formula(&mut data, wide, &mut bytes);

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

    (
        format!("mov {effective_address_formula}, {immediate}"),
        bytes,
    )
}

fn mov_accumulator_to_memory(
    byte: u8,
    mut bytes: std::slice::Iter<u8>,
) -> (String, std::slice::Iter<u8>) {
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

    (format!("mov [{address}], ax"), bytes)
}

fn mov_memory_to_accumulator(
    byte: u8,
    mut bytes: std::slice::Iter<u8>,
) -> (String, std::slice::Iter<u8>) {
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

    (format!("mov ax, [{address}]"), bytes)
}

fn mov_immediate_to_reg(
    byte: u8,
    mut bytes: std::slice::Iter<u8>,
) -> (String, std::slice::Iter<u8>) {
    let mut data: Vec<u8> = Vec::new();
    let wide = (byte >> 3) & 1 == 1;
    let reg = get_register_encoding(byte & 7, wide).unwrap();
    //println!("; MOV immediate to register: {reg}, wide: {w}");
    data.push(*bytes.next().unwrap());

    let immediate: i16 = if wide {
        data.push(*bytes.next().unwrap());

        i16::from_ne_bytes(data[3..5].try_into().unwrap())
    } else {
        i8::from_ne_bytes(data[3..4].try_into().unwrap())
            .try_into()
            .unwrap()
    };

    (format!("mov {reg}, {immediate}"), bytes)
}
