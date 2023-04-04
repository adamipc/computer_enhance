use crate::cpu::Cpu;
use std::env;
use std::fs::File;

pub mod cpu;

fn main() -> std::io::Result<()> {
    let mut args = env::args();
    args.next();

    let filename = args.next().unwrap();
    println!("; {filename}");

    let mut f = File::open(filename)?;

    let mut cpu = Cpu::default();

    let read_len = cpu.read_file_into_memory(&mut f, 0)?;

    println!("; Instruction stream is {} bytes long.", read_len);

    // Tell NASM to encode our ASM using basic 16bit instructions
    // so we can easily re-encode our decode to test.
    println!("\nbits 16\n");

    cpu.run(false);
    Ok(())
}
