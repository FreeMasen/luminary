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
use rand::Rng;

#[derive(Parser, Debug)]
struct Args {
    input: PathBuf,
    #[arg(short, long)]
    output: Option<PathBuf>,
    #[arg(long, default_value_t = FileType::Exe)]
    filetype: FileType,
    #[arg(long)]
    intermediate_dir: Option<PathBuf>,
    #[arg(long, short = 'O', default_value_t = 0)]
    opt: u8,
    #[arg(long = "runtime", short)]
    runtime_location: Option<PathBuf>,
    #[arg(long, short = 'L')]
    location: Vec<PathBuf>,
    #[arg(long, short = 'l')]
    library: Vec<String>,
    #[arg(long, short)]
    force: bool,
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
    env_logger::init();
    let args = Args::parse();
    tracing::warn!("ARGS: {args:#?}");
    let Args {
        input,
        output,
        filetype,
        intermediate_dir,
        opt,
        runtime_location,
        library,
        location,
        force,
    } = args;

    let context = Context::create();
    let module = luminary::run_on(&context, input.clone());
    module.verify().unwrap_or_else(|e| {
        if std::env::var("LUMINARY_DEBUG_OUTPUT_LL")
            .map(|v| v != "0")
            .unwrap_or(false)
        {
            let mut dest = get_dest(output.as_ref());
            dest.write_all(module.to_string().as_bytes()).unwrap();
        }
        if !force {
            panic!("Failed to run {e}");
        } else {
            eprintln!("Warning invalid llvm module produced: `{e}`");
        }
    });
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
            #[cfg(target_os = "windows")]
            let obj_ext = ".obj";
            #[cfg(not(target_os = "windows"))]
            let obj_ext = ".o";

            let obj = run_llc(LlvmFileType::Object, &module, opt);

            let (tmp_path, _tmp) = if let Some(tmp) = intermediate_dir {
                std::fs::create_dir_all(&tmp).ok();
                let mut rng = rand::thread_rng();
                let rnd_name: String = (0..5)
                    .map(|_| {
                        char::from(if rng.r#gen() {
                            rng.gen_range(b'a'..=b'z')
                        } else if rng.r#gen() {
                            rng.gen_range(b'A'..=b'Z')
                        } else {
                            rng.gen_range(b'0'..=b'9')
                        })
                    })
                    .collect();
                let tmp_path = tmp.join(format!("{rnd_name}{obj_ext}"));
                tracing::debug!("tmp: {}", tmp_path.display());
                std::fs::File::create(&tmp_path).unwrap();
                (tmp_path, None)
            } else {
                let tmp_o = tempfile::Builder::new().suffix(obj_ext).tempfile().unwrap();
                let p = tmp_o.path().to_path_buf();
                (p, Some(tmp_o))
            };
            std::fs::write(&tmp_path, obj.as_slice()).unwrap();

            let (dest, tmp_file) = if let Some(dest_path) = output.as_ref() {
                (dest_path.clone(), None)
            } else {
                let tmp2 = tempfile::Builder::new().suffix(obj_ext).tempfile().unwrap();
                (tmp2.path().to_owned(), Some(tmp2))
            };
            link_exe(
                tmp_path.as_path(),
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

#[cfg(unix)]
fn link_exe(
    obj_path: &Path,
    dest: &PathBuf,
    runtime_path: Option<&PathBuf>,
    library: &[String],
    location: &[PathBuf],
) {
    if !obj_path.exists() {
        panic!("object path does not exist at {}", obj_path.display());
    }
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
        let runtime_path = dunce::canonicalize(runtime_path).unwrap();
        cmd.arg("-L").arg(&runtime_path);
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
            eprint!(r#" "{}""#, arg.to_str().unwrap())
        }
        eprintln!();
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

/*
"C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise\\VC\\Tools\\MSVC\\14.40.33807\\bin\\Hostx64\\x64\\link.exe"
"-out:D:\\a\\luminary\\luminary\\target\\tmp\\linking_works\\app"
-defaultlib:libcmt
-defaultlib:oldnames
"-libpath:C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise\\VC\\Tools\\MSVC\\14.40.33807\\lib\\x64"
"-libpath:C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise\\VC\\Tools\\MSVC\\14.40.33807\\atlmfc\\lib\\x64"
"-libpath:C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.22621.0\\ucrt\\x64"
"-libpath:C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.22621.0\\um\\x64"
"-libpath:C:\\Program Files\\LLVM\\lib\\clang\\18\\lib\\windows"
"-libpath:D:\\a\\luminary\\luminary\\target\\tmp\\slib"
-nologo "D:\\a\\luminary\\luminary\\target\\tmp\\linking_works\\inter\\FfTq6.obj"
luminary_runtime.lib
*/
#[cfg(windows)]
fn link_exe(
    obj_path: &Path,
    dest: &PathBuf,
    runtime_path: Option<&PathBuf>,
    library: &[String],
    location: &[PathBuf],
) {
    if !obj_path.exists() {
        panic!("object path does not exist at {}", obj_path.display());
    }
    let mut cmd = Command::new("link.exe");
    cmd.arg(&format!("/OUT:{}", dest.display()))
        .arg("/DEFAULTLIB:libcmt")
        .arg("/DEFAULTLIB:oldnames")
        .arg("/NOLOGO")
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    let link_verbose = std::env::var("LUMINARY_USE_VERBOSE_LINK")
        .map(|s| !s.is_empty() && s != "0")
        .unwrap_or(false);
    if link_verbose {
        cmd.arg("/VERBOSE");
    }
    // if let Ok(path) = std::env::var("PATH") {
    //     let roots = std::collections::HashSet::new();
    //     for p in path.split(":") {
    //         let p = PathBuf::from(p);
    //         if let Some(drive) = p.ancestors().find(|s| s.ends_with(':')) {
    //             roots.insert(drive);
    //         }
    //     }
    // }
    let runtime_file_name = if let Some(runtime_path) = runtime_path {
        let runtime_path = dunce::canonicalize(runtime_path).unwrap();
        cmd.arg(&format!("/LIBPATH:{}", runtime_path.display()));
        let ext = std::fs::read_dir(&runtime_path)
            .unwrap()
            .find_map(|e| {
                let e = e.ok()?;
                e.path().extension().and_then(|ext| {
                    let ext = ext.to_str()?;
                    (ext == "dll" || ext == "lib").then(|| ext.to_string())
                })
            })
            .unwrap_or_else(|| "lib".to_string());
        format!("luminary_runtime.{ext}")
    } else {
        "luminary_runtime.lib".to_string()
    };
    for l in location {
        cmd.arg(&format!("/LIBPATH:{}", l.display()));
    }
    for l in library {
        cmd.arg(l);
    }
    cmd.arg(runtime_file_name);
    let output = cmd.spawn().unwrap().wait_with_output().unwrap();
    if !output.status.success() {
        eprint!("clang");
        for arg in cmd.get_args() {
            eprint!(r#" "{}""#, arg.to_str().unwrap())
        }
        eprintln!();
        eprintln!("linking with clang failed with the following output:");
        std::fs::copy(obj_path, "failed-link.o").ok();

        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        if !stdout.is_empty() {
            eprintln!("{stdout}",);
        }
        if !stderr.is_empty() {
            eprintln!("{stderr}");
        }
        if !link_verbose {
            eprintln!("NOTE: set LUMINARY_USE_VERBOSE_CLANG=1 for more details");
        }
        if runtime_path.is_none() {
            eprintln!("HINT: setting the argument --runtime-location to the directory containing libluminary_runtime.a|o might help");
        }
        std::process::exit(1);
    } else {
        println!("{}", String::from_utf8_lossy(&output.stdout));
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
                .truncate(true)
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
