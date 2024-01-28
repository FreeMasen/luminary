use std::{
    fmt::Display,
    fs::{File, OpenOptions},
    io::{Read, Stdout, Write},
    path::{Path, PathBuf},
    process::{Command, Stdio},
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
    #[arg(long, short)]
    runtime_location: Option<PathBuf>,
    #[arg(long, short = 'L')]
    location: Vec<PathBuf>,
    #[arg(long, short = 'l')]
    library: Vec<String>,
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
        runtime_location,
        library,
        location,
    } = Args::parse();
    let context = Context::create();
    let module = luminary::run_on(&context, input.clone());
    module.verify().unwrap();
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
            link_exe(
                tmp_o.path(),
                &dest,
                runtime_location.as_ref(),
                &library,
                &location,
            );
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

fn link_exe(
    obj_path: &Path,
    dest: &PathBuf,
    runtime_path: Option<&PathBuf>,
    library: &[String],
    location: &[PathBuf],
) {
    let mut cmd = Command::new("clang");
    cmd.arg(obj_path)
        .arg("-o")
        .arg(dest)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    let clang_verbose = std::env::var("LUMINARY_USE_VERBOSE_CLANG")
        .map(|s| !s.is_empty() && s != "0")
        .unwrap_or(false);
    if clang_verbose {
        cmd.arg("--verbose");
    }
    if let Some(runtime_path) = runtime_path {
        let runtime_path = runtime_path.canonicalize().expect("valid runtime path");
        cmd.arg(format!("-L{}", runtime_path.display()));
    }
    for l in library {
        cmd.arg("-l").arg(l);
    }
    for l in location {
        cmd.arg("-L").arg(l);
    }
    #[cfg(target_os = "linux")]
    cmd.arg("-lm");
    cmd.arg("-lluminary_runtime");
    let child = cmd.spawn().unwrap();
    let clang_outout = child.wait_with_output().unwrap();
    if !clang_outout.status.success() {
        eprint!("clang");
        for arg in cmd.get_args() {
            eprint!(" {}", arg.to_str().unwrap())
        }
        eprintln!("");
        eprintln!("linking with clang failed with the following output:");
        std::fs::copy(obj_path, "failed-link.o").ok();

        let stdout = String::from_utf8_lossy(&clang_outout.stdout);
        let stderr = String::from_utf8_lossy(&clang_outout.stderr);
        if !stdout.is_empty() {
            eprintln!("{stdout}",);
        }
        if !stderr.is_empty() {
            eprintln!("{stderr}");
        }
        if !clang_verbose {
            eprintln!("NOTE: set LUMINARY_USE_VERBOSE_CLANG=1 for more details");
        }
        if runtime_path.is_none() {
            eprintln!("HINT: setting the argument --runtime-location to the directory containing libluminary_runtime.a|o might help");
        }
        std::process::exit(1);
    } else {
        println!("{}", String::from_utf8_lossy(&clang_outout.stdout));
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
