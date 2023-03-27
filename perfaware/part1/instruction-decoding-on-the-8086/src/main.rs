use std::env;
use std::fs::File;
use std::io;
use std::io::Read;

const OPCODES: [(u8, (&str, usize)); 1] = [(0b10001000, ("mov", 1))];
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
    while let Some((i, byte)) = e.next() {
        println!("; {i}: {byte:#b}");
        let (instruction, datalen) = get_instruction(*byte).unwrap();
        println!("; Got instruction: {instruction}. Need to read {datalen} bytes of data.");
        let word = is_bit_set(*byte, 1);
        let reg_is_destination = is_bit_set(*byte, 2);
        println!(
            "; Instruction operates on word: {}, REG is destination: {}",
            word, reg_is_destination
        );

        let mut data = Vec::new();
        for _ in [0..datalen] {
            match e.next() {
                Some((i, byte)) => {
                    println!("; {i}: {byte:#b}");
                    data.push(byte)
                }
                None => panic!("Couldn't read enough data."),
            }
        }

        match instruction {
            "mov" => match (data[0] & 0b1100_0000).rotate_left(2) {
                0b11 => {
                    let reg = get_register_encoding((*data[0] & 0b0011_1000).rotate_right(3), word)
                        .unwrap();
                    let rm = get_register_encoding(*data[0] & 0b0000_0111, word).unwrap();
                    println!("; reg: {reg}, rm: {rm}");
                    if reg_is_destination {
                        println!("mov {reg}, {rm}");
                    } else {
                        println!("mov {rm}, {reg}");
                    }
                }
                n => panic!("Memory mode for MOV not implemented: {n:#b}"),
            },
            &_ => todo!(),
        }
    }

    Ok(())
}

fn is_bit_set(byte: u8, bit: usize) -> bool {
    (byte >> bit - 1) & 0x1 == 0x1
}

fn get_register_encoding(byte: u8, word: bool) -> Option<&'static str> {
    REGISTER_ENCODING
        .iter()
        .filter_map(|(key, val)| {
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
        .next()
}

fn get_instruction(byte: u8) -> Option<(&'static str, usize)> {
    for (mask, instruction) in OPCODES {
        if mask == (byte & 0b11111100) {
            return Some(instruction);
        }
    }

    None
}
