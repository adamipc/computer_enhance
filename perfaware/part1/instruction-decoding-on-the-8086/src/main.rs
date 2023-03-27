use std::env;
use std::fs::File;
use std::io;
use std::io::Read;

const OPCODES: [(u8, (&str, usize)); 4] = [
    (0b10001000, ("mov-byte-reg-as-src", 1)),
    (0b10001001, ("mov-word-reg-as-src", 1)),
    (0b10001010, ("mov-byte-reg-as-dest", 1)),
    (0b10001011, ("mov-word-reg-as-dest", 1)),
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
        println!("Got instruction: {instruction}. Need to read {datalen} bytes of data.");
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
    }

    Ok(())
}

fn get_instruction(byte: u8) -> Option<(&'static str, usize)> {
    for (mask, instruction) in OPCODES {
        if mask == byte {
            return Some(instruction);
        }
    }

    None
}
