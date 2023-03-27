use std::env;
use std::fs::File;
use std::io;
use std::io::Read;

const OPCODES: [(u8, (&str, usize)); 1] = [(0b10001000, ("mov", 1))];
const REG_ENCODING: [(u8, (&str, &str)); 8] = [
    (0x0, ("al", "ax")),
    (0x1, ("cl", "cx")),
    (0x2, ("dl", "dx")),
    (0x3, ("bl", "bx")),
    (0x4, ("ah", "sp")),
    (0x5, ("ch", "bp")),
    (0x6, ("dh", "si")),
    (0x7, ("bh", "di")),
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
        //println!("; {i}: {byte:#b}");
        let (instruction, datalen) = get_instruction(*byte).unwrap();
        //println!("; Got instruction: {instruction}. Need to read {datalen} bytes of data.");
        let reg_is_destination = byte & 0x2 == 0x2;
        let operates_on_word = byte & 0x1 == 0x1;
        //println!("; REG is destination: {reg_is_destination}, Operates on word: {operates_on_word}");
        let mut data = Vec::new();
        for _ in [0..datalen] {
            match e.next() {
                Some((i, byte)) => {
                    //println!("; data {i}: {byte:#b}");
                    data.push(byte)
                }
                None => panic!("Couldn't read enough data."),
            }
        }

        match instruction {
            "mov" => {
                let reg = get_reg_encoding((*data[0] & 0b00111000) >> 3, operates_on_word).unwrap();
                let rm = get_reg_encoding(*data[0] & 0b00000111, operates_on_word).unwrap();
                //println!("; Got REG/RM registers: {reg}/{rm}");
                if reg_is_destination {
                    println!("mov {reg}, {rm}");
                } else {
                    println!("mov {rm}, {reg}");
                }
            }
            &_ => todo!(),
        }
    }

    Ok(())
}

fn get_reg_encoding(byte: u8, operates_on_word: bool) -> Option<&'static str> {
    for (reg_value, (reg_byte, reg_word)) in REG_ENCODING {
        if byte == reg_value {
            if operates_on_word {
                return Some(reg_word);
            } else {
                return Some(reg_byte);
            }
        }
    }

    None
}

fn get_instruction(byte: u8) -> Option<(&'static str, usize)> {
    for (opcode, instruction) in OPCODES {
        if (byte & 0b11111100) == opcode {
            return Some(instruction);
        }
    }

    None
}
