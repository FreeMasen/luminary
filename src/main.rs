use std::{
    fmt::Display,
    fs::{File, OpenOptions},
    io::{Read, Stdout, Write},
    path::{Path, PathBuf},
    process::Command,
};

use clap::{Parser, ValueEnum};
use inkwell::{
    context::Context,
    memory_buffer::MemoryBuffer,
    module::Module,
    targets::{
        CodeModel, FileType as LlvmFileType, InitializationConfig, RelocMode, Target, TargetMachine,
    },
    OptimizationLevel,
};

#[derive(Parser, Debug)]
struct Args {
    input: PathBuf,
    #[arg(short, long)]
    output: Option<PathBuf>,
    #[arg(long, default_value_t = FileType::Exe)]
    filetype: FileType,
    #[arg(long, short = 'O', default_value_t = 0)]
    opt: u8,
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
        opt,
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
            let obj = run_llc(LlvmFileType::Assembly, &module, opt);
            if let Some(dest_path) = output.as_ref() {
                std::fs::write(dest_path, obj.as_slice()).unwrap();
            } else {
                let mut out = std::io::stdout();
                out.write_all(obj.as_slice()).unwrap();
            }
        }
        FileType::Lib => {
            let obj = run_llc(LlvmFileType::Object, &module, opt);

            if let Some(dest_path) = output.as_ref() {
                std::fs::write(dest_path, obj.as_slice()).unwrap();
            } else {
                let mut out = std::io::stdout();
                out.write_all(obj.as_slice()).unwrap();
            }
        }
        FileType::Exe => {
            let obj = run_llc(LlvmFileType::Object, &module, opt);
            let tmp_o = tempfile::Builder::new().suffix(".o").tempfile().unwrap();
            std::fs::write(tmp_o.path(), obj.as_slice()).unwrap();

            let (dest, tmp_file) = if let Some(dest_path) = output.as_ref() {
                (dest_path.clone(), None)
            } else {
                let tmp2 = tempfile::Builder::new().suffix(".o").tempfile().unwrap();
                (tmp2.path().to_owned(), Some(tmp2))
            };
            link_exe(tmp_o.path(), &dest);
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

#[cfg(all(target_os = "linux", feature = "link-with-ld"))]
fn link_exe(obj_path: &Path, dest: &PathBuf) {
    let mut cmd = Command::new("ld");
    cmd.arg(obj_path)
        .arg("/lib/x86_64-linux-gnu/Scrt1.o")
        .arg("-o")
        .arg(dest)
        .arg("-pie")
        .arg("-z")
        .arg("relro")
        .arg("--hash-style=gnu")
        .arg("--build-id")
        .arg("--eh-frame-hdr")
        .arg("-m")
        .arg("elf_x86_64")
        .arg("-dynamic-linker")
        .arg("/lib64/ld-linux-x86-64.so.2")
        .arg("-L")
        .arg("/usr/lib/x86_64-linux-gnu")
        .arg("-l")
        .arg("m")
        .arg("-l")
        .arg("c");
    let child = cmd.spawn().unwrap();
    let ld_outout = child.wait_with_output().unwrap();
    if !ld_outout.status.success() {
        println!("LDOUT: {}", String::from_utf8_lossy(&ld_outout.stdout));
        eprintln!("LDERR: {}", String::from_utf8_lossy(&ld_outout.stderr));
        std::process::exit(1);
    } else if !ld_outout.stdout.is_empty() {
        println!("{}", String::from_utf8_lossy(&ld_outout.stdout));
    }
}

#[cfg(not(all(target_os = "linux", feature = "link-with-ld")))]
fn link_exe(obj_path: &Path, dest: &PathBuf) {
    let mut cmd = Command::new("cc");
    cmd.arg("-o").arg(dest);
    if std::env::var("LUMINARY_USE_VERBOSE_CC")
        .map(|s| !s.is_empty() && s != "0")
        .unwrap_or(false)
    {
        cmd.arg("--verbose");
    }
    cmd.arg("-lm").arg(obj_path);
    let child = cmd.spawn().unwrap();
    let clang_outout = child.wait_with_output().unwrap();
    if !clang_outout.status.success() {
        println!("{}", String::from_utf8_lossy(&clang_outout.stdout));
        eprintln!("{}", String::from_utf8_lossy(&clang_outout.stderr));
        std::process::exit(1);
    } else {
        if !clang_outout.stdout.is_empty() {
            println!("{}", String::from_utf8_lossy(&clang_outout.stdout));
        }
        if !clang_outout.stderr.is_empty() {
            eprintln!("{}", String::from_utf8_lossy(&clang_outout.stderr));
        }
    }
}

fn run_llc(file_type: LlvmFileType, module: &Module, opt: u8) -> MemoryBuffer {
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
    let opt = match opt {
        0 => OptimizationLevel::None,
        1 => OptimizationLevel::Less,
        2 => OptimizationLevel::Aggressive,
        _ => OptimizationLevel::Default,
    };
    let reloc = RelocMode::Default;
    let model = CodeModel::Default;
    let cpu = TargetMachine::get_host_cpu_name();
    let cpu = cpu.to_str().unwrap();
    let features = TargetMachine::get_host_cpu_features();
    let features = features.to_str().unwrap();
    let machine = target
        .create_target_machine(&trip, cpu, features, opt, reloc, model)
        .unwrap();
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
