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
    while let Some((_i, byte)) = e.next() {
        //println!("; {_i}: {byte:#b}");
        let (instruction, datalen) = get_instruction(*byte).unwrap();
        //println!("; Got instruction: {instruction}. Need to read {datalen} bytes of data.");
        let reg_is_destination = byte & 0x2 == 0x2;
        let operates_on_word = byte & 0x1 == 0x1;
        //println!("; REG is destination: {reg_is_destination}, Operates on word: {operates_on_word}");
        let mut data = Vec::new();
        for _ in [0..datalen] {
            match e.next() {
                Some((_i, byte)) => {
                    //println!("; data {_i}: {byte:#b}");
                    data.push(byte)
                }
                None => panic!("Couldn't read enough data."),
            }
        }

        match instruction {
            "mov" => match (data[0] & 0b1100_0000).rotate_left(2) {
                0b11 => {
                    let reg = get_register_encoding(
                        (*data[0] & 0b0011_1000).rotate_right(3),
                        operates_on_word,
                    )
                    .unwrap();
                    let rm =
                        get_register_encoding(*data[0] & 0b0000_0111, operates_on_word).unwrap();
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

fn get_instruction(byte: u8) -> Option<(&'static str, usize)> {
    for (opcode, instruction) in OPCODES {
        if (byte & 0b11111100) == opcode {
            return Some(instruction);
        }
    }

    None
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
