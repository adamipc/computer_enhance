use std::env;
use std::fs::File;
use std::io;
use std::io::Read;

const OPCODES: [(u8, u8, &str); 2] = [
    (0b10001000, 0b1111_1100, "mov-reg_mem-to_from-reg"),
    (0b10110000, 0b1111_0000, "mov-immediate-to-reg"),
];
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
    let mut e = buffer.iter().enumerate();
    while let Some((_i, byte)) = e.next() {
        println!("; {_i}: {byte:#b}");
        let instruction = get_instruction(*byte).unwrap();
        println!("; Got instruction: {instruction}.");

        let mut data: Vec<u8> = Vec::new();
        match instruction {
            "mov-reg_mem-to_from-reg" => {
                let reg_is_destination = byte & 0x2 == 0x2;
                let w = byte & 0x1 == 0x1;
                //println!("; REG is destination: {reg_is_destination}, Operates on word: {w}");
                if let Some((_, byte)) = e.next() {
                    data.push(*byte);
                }
                let mod_field = (data[0] & 0b1100_0000).rotate_left(2);
                match mod_field {
                    0b11 => {
                        let reg = get_register_encoding((data[0] & 0b0011_1000).rotate_right(3), w)
                            .unwrap();
                        let rm = get_register_encoding(data[0] & 0b0000_0111, w).unwrap();
                        //println!("; reg: {reg}, rm: {rm}");
                        if reg_is_destination {
                            println!("mov {reg}, {rm}");
                        } else {
                            println!("mov {rm}, {reg}");
                        }
                    }
                    n => panic!("Memory mode for MOV not implemented: {n:#b}"),
                }
            }
            "mov-immediate-to-reg" => {
                let w = (byte >> 3) & 1 == 1;
                let reg = get_register_encoding(byte & 7, w).unwrap();
                println!("; MOV immediate to register: {reg}, wide: {w}");

                if let Some((_, byte)) = e.next() {
                    data.push(*byte);
                }

                if w {
                    if let Some((_, byte)) = e.next() {
                        data.push(*byte);
                    }
                    match data[0..2].try_into() {
                        Ok(bytes) => {
                            let immediate = i16::from_ne_bytes(bytes);
                            println!("mov {reg}, {immediate}");
                        }
                        Err(err) => {
                            panic!("Got error trying to convert {data:?} to an i16: {err}")
                        }
                    }
                } else {
                    match data[0..1].try_into() {
                        Ok(bytes) => {
                            let immediate = i8::from_ne_bytes(bytes);
                            println!("mov {reg}, {immediate}");
                        }
                        Err(err) => {
                            panic!("Got error trying to convert {data:?} to an i8: {err}")
                        }
                    }
                }
            }
            &_ => todo!(),
        }
    }

    Ok(())
}

fn get_instruction(byte: u8) -> Option<&'static str> {
    for (opcode, mask, instruction) in OPCODES {
        if (byte & mask) == opcode {
            return Some(instruction);
        }
    }

    panic!("Couldn't find instruction for byte {byte:#b}");
}

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
