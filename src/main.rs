use std::{
    fmt::Display,
    fs::{File, OpenOptions},
    io::{Read, Stdout, Write},
    path::{Path, PathBuf},
    process::{Command, Output as ChildOutput, Stdio},
};

use clap::{Parser, ValueEnum};
use inkwell::{context::Context, module::Module, targets::{TargetMachine, Target, InitializationConfig, RelocMode, CodeModel, FileType as LlvmFileType}, OptimizationLevel, memory_buffer::MemoryBuffer};

#[derive(Parser, Debug)]
struct Args {
    input: PathBuf,
    #[arg(short, long)]
    output: Option<PathBuf>,
    #[arg(long, default_value_t = FileType::Exe)]
    filetype: FileType,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum FileType {
    /// .s file
    Asm,
    /// LLVM IR file
    Ll,
    /// LlVM bitcode file
    Bc,
    /// Unlinked object file
    Lib,
    /// executable object file
    Exe,
}

impl Default for FileType {
    fn default() -> Self {
        Self::Exe
    }
}

impl Display for FileType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                FileType::Asm => "asm",
                FileType::Ll => "ll",
                FileType::Bc => "bc",
                FileType::Lib => "lib",
                FileType::Exe => "exe",
            }
        )
    }
}

fn main() {
    let Args {
        input,
        output,
        filetype,
    } = Args::parse();
    let context = Context::create();
    let module = luminary::run_on(&context, input.clone());

    match filetype {
        FileType::Ll => {
            let mut dest = get_dest(output.as_ref());
            dest.write_all(module.to_string().as_bytes()).unwrap();
        }
        FileType::Bc => {
            let mut dest = get_dest(output.as_ref());
            let bc = module.write_bitcode_to_memory();
            dest.write_all(bc.as_slice()).unwrap();
        }
        FileType::Asm => {
            let obj = run_llc(LlvmFileType::Assembly, &module);
            if let Some(dest_path) = output.as_ref() {
                std::fs::write(dest_path, obj.as_slice()).unwrap();
            } else {
                let mut out = std::io::stdout();
                out.write_all(obj.as_slice()).unwrap();
            }
        }
        FileType::Lib => {
            let obj = run_llc(LlvmFileType::Object, &module);
            
            if let Some(dest_path) = output.as_ref() {
                std::fs::write(dest_path, obj.as_slice()).unwrap();
            } else {
                let mut out = std::io::stdout();
                out.write_all(obj.as_slice()).unwrap();
            }
        }
        FileType::Exe => {
            let obj = run_llc(LlvmFileType::Object, &module);
            let tmp_o = tempfile::Builder::new().suffix(".o").tempfile().unwrap();
            std::fs::write(tmp_o.path(), obj.as_slice()).unwrap();
            let mut cmd = Command::new("clang");
            cmd.arg("-o");
            let tmp_file = if let Some(dest_path) = output.as_ref() {
                cmd.arg(dest_path);
                None
            } else {
                let tmp2 = tempfile::Builder::new().suffix(".o").tempfile().unwrap();
                cmd.arg(tmp_o.path());
                Some(tmp2)
            };
            cmd.arg("-lm").arg(tmp_o.path());
            let child = cmd.spawn().unwrap();
            let clang_outout = child.wait_with_output().unwrap();
            if !clang_outout.status.success() {
                println!("{}", String::from_utf8_lossy(&clang_outout.stdout));
                eprintln!("{}", String::from_utf8_lossy(&clang_outout.stderr));
                std::process::exit(1);
            }
            if let Some(mut tmp) = tmp_file {
                let mut out = std::io::stdout();
                loop {
                    let mut buf = [0u8; 4096];
                    let size = tmp.read(&mut buf).unwrap();
                    if size == 0 {
                        break;
                    }
                    out.write_all(&buf[..size]).unwrap();
                }
            }
        }
    }
}

fn run_llc(file_type: LlvmFileType, module: &Module) -> MemoryBuffer {
    let trip = TargetMachine::get_default_triple();
    Target::initialize_all(&InitializationConfig {
        asm_parser: true,
        asm_printer: true,
        base: true,
        disassembler: false,
        info: true,
        machine_code: true,
    });
    let target = Target::from_triple(&trip).unwrap();
    let opt = OptimizationLevel::default();
    let reloc = RelocMode::Default;
    let model = CodeModel::Default;
    let cpu = TargetMachine::get_host_cpu_name();
    let cpu = cpu.to_str().unwrap();
    let features = TargetMachine::get_host_cpu_features();
    let features = features.to_str().unwrap();
    let machine = target.create_target_machine(&trip, cpu, features, opt, reloc, model).unwrap();
    machine.write_to_memory_buffer(module, file_type).unwrap()
}

fn get_dest(dest: Option<&PathBuf>) -> Output {
    if let Some(dest) = dest {
        Output::File(
            OpenOptions::new()
                .create(true)
                .write(true)
                .open(dest)
                .unwrap(),
        )
    } else {
        Output::StdIo(std::io::stdout())
    }
}

pub enum Output {
    File(File),
    StdIo(Stdout),
}

impl Write for Output {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            Output::File(f) => f.write(buf),
            Output::StdIo(s) => s.write(buf),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            Output::File(f) => f.flush(),
            Output::StdIo(s) => s.flush(),
        }
    }
}
