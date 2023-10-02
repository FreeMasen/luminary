use std::{path::{PathBuf, Path}, process::{Command, Stdio, Output as ChildOutput}, io::{Write, Stdout, Read}, fs::File, fmt::Display};

use clap::{Parser, ValueEnum};
use inkwell::{module::Module, context::Context};

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
        write!(f, "{}", match self {
            FileType::Asm => "asm",
            FileType::Ll => "ll",
            FileType::Bc => "bc",
            FileType::Lib => "lib",
            FileType::Exe => "exe",
        })
    }
}

fn main() {
    let Args { input, output, filetype } = Args::parse();
    let context = Context::create();
    let module = luminary::run_on(&context, input.clone());
    
    match filetype {
        FileType::Ll => {
            let mut dest = get_dest(output.as_ref());
            dest.write_all(module.to_string().as_bytes()).unwrap();
        },
        FileType::Bc => {
            let mut dest = get_dest(output.as_ref());
            let bc = module.write_bitcode_to_memory();
            dest.write_all(bc.as_slice()).unwrap();
        },
        FileType::Asm => {
            let mut tmp_s_file = tempfile::Builder::new().suffix(".s").tempfile().unwrap();
            let llc_output = run_llc("asm", &module, tmp_s_file.path());
            if !llc_output.status.success() {
                println!("{}", String::from_utf8_lossy(&llc_output.stdout));
                eprintln!("{}", String::from_utf8_lossy(&llc_output.stderr));
                std::process::exit(1);
            }
            if let Some(dest_path) = output.as_ref() {
                std::fs::copy(tmp_s_file.path(), dest_path).unwrap();
            } else {
                let mut out = std::io::stdout();
                loop {
                    let mut buf = [0u8;4096];
                    let size = tmp_s_file.read(&mut buf).unwrap();
                    if size == 0 {
                        break;
                    }
                    out.write_all(&buf[..size]).unwrap();
                }
            }            
        },
        FileType::Lib => {
            let (output, tmp_file) = if let Some(dest_path) = output.as_ref() {
                let output = run_llc("obj", &module, dest_path);
                (output, None)
            } else {
                let tmp_o = tempfile::Builder::new().suffix(".o").tempfile().unwrap();
                let output = run_llc("obj", &module, tmp_o.path());
                (output, Some(tmp_o))
            };

            if !output.status.success() {
                println!("{}", String::from_utf8_lossy(&output.stdout));
                eprintln!("{}", String::from_utf8_lossy(&output.stderr));
                std::process::exit(1);
            }
            
            if let Some(mut tmp) = tmp_file {
                let mut out = std::io::stdout();
                loop {
                    let mut buf = [0u8;4096];
                    let size = tmp.read(&mut buf).unwrap();
                    if size == 0 {
                        break;
                    }
                    out.write_all(&buf[..size]).unwrap();
                }
            }
        },
        FileType::Exe => {
            let tmp_o = tempfile::Builder::new().suffix(".o").tempfile().unwrap();
            let llc_output = run_llc("obj", &module, tmp_o.path());
            if !llc_output.status.success() {
                println!("{}", String::from_utf8_lossy(&llc_output.stdout));
                eprintln!("{}", String::from_utf8_lossy(&llc_output.stderr));
                std::process::exit(1);
            }
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
            cmd.arg("-no-pie")
                .arg("-lm")
                .arg(tmp_o.path());
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
                    let mut buf = [0u8;4096];
                    let size = tmp.read(&mut buf).unwrap();
                    if size == 0 {
                        break;
                    }
                    out.write_all(&buf[..size]).unwrap();
                }
            }
        },
    }
}

fn run_llc(
    file_type: &str,
    module: &Module,
    dest_path: &Path,
) -> ChildOutput {
    let mut cmd = Command::new("llc");
    cmd
        .arg("-filetype")
        .arg(file_type)
        .arg("-o")
        .arg(dest_path)
        .stdin(Stdio::piped());
    let mut cmp_task = cmd.spawn().expect("llc");
    let stdin = cmp_task.stdin.as_mut().expect("stdin");
    stdin.write_all(module.to_string().as_bytes()).unwrap();
    cmp_task.wait_with_output().unwrap()
}

fn get_dest(dest: Option<&PathBuf>) -> Output {
    if let Some(dest) = dest {
        Output::File(File::open(dest).unwrap())
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
