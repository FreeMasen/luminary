#![allow(dead_code)]
use std::{
    fmt,
    path::{Path, PathBuf},
    process::{Command, Output, Stdio},
    sync::OnceLock,
    time::SystemTime,
};

use escargot::format::Message;

static LIB_PATHS: OnceLock<(PathBuf, PathBuf)> = OnceLock::new();
#[cfg(target_os = "macos")]
const STATIC_EXT: &str = "a";
#[cfg(target_os = "macos")]
const DYNAMIC_EXT: &str = "dylib";
#[cfg(target_os = "linux")]
const STATIC_EXT: &str = "a";
#[cfg(target_os = "linux")]
const DYNAMIC_EXT: &str = "so";
#[cfg(target_os = "windows")]
const STATIC_EXT: &str = "lib";
#[cfg(target_os = "windows")]
const DYNAMIC_EXT: &str = "dll";

#[derive(Debug)]
pub struct TestConfig {
    pub base_dir: PathBuf,
    pub static_runtime: PathBuf,
    pub dynamic_runtime: PathBuf,
    pub cmd: PathBuf,
}

pub fn setup(name: &str) -> TestConfig {
    let temp_dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR"));
    let (static_runtime, dynamic_runtime) = LIB_PATHS
        .get_or_init(|| {
            let mut static_path: Option<PathBuf> = None;
            let mut dynamic_path: Option<PathBuf> = None;
            let runtime_build = escargot::CargoBuild::new()
                .features("runtime")
                .arg("-p")
                .arg("luminary-runtime")
                .release()
                .exec()
                .expect("building runtime failed");
            for msg in runtime_build {
                let msg = msg.expect("invalid message...");
                let msg = msg.decode().expect("invalid message...");
                if let Message::CompilerArtifact(art) = msg {
                    for file in &art.filenames {
                        let Some(name) = file.file_stem() else {
                            continue;
                        };
                        let Some(name) = name.to_str() else {
                            continue;
                        };
                        if !name.ends_with("luminary_runtime") {
                            continue;
                        }
                        let Some(ext) = file.extension() else {
                            continue;
                        };
                        if ext == STATIC_EXT {
                            static_path = Some(file.to_path_buf());
                            continue;
                        }
                        if ext == DYNAMIC_EXT {
                            dynamic_path = Some(file.to_path_buf());
                            continue;
                        }
                    }
                }
            }
            let copy_and_create = |dir_name: &str, src: &Path| -> PathBuf {
                let lib_path = temp_dir.join(dir_name);
                std::fs::create_dir_all(&lib_path).unwrap();
                let dest = lib_path.join(src.file_name().unwrap());
                std::fs::copy(src, &dest).unwrap_or_else(|e| {
                    panic!(
                        "Failed to copy `{}`({}) to `{}`({}): {e}\n",
                        src.display(),
                        src.exists(),
                        dest.display(),
                        dest.exists(),
                    );
                });
                dest
            };
            let dynamic_runtime = dynamic_path.expect("Didn't generate a dynamic runtime lib");
            let static_runtime = static_path.expect("Didn't generate a static runtime");

            (
                copy_and_create("lib", &static_runtime),
                copy_and_create("slib", &dynamic_runtime),
            )
        })
        .clone();
    let base_dir = temp_dir.join(name);
    std::fs::create_dir_all(&base_dir).unwrap();
    TestConfig {
        dynamic_runtime,
        static_runtime,
        cmd: PathBuf::from(env!("CARGO_BIN_EXE_luminary")),
        base_dir,
    }
}

#[track_caller]
pub fn check_return_code(output: &Output, code: i32) {
    if output.status.code() == Some(code) {
        return;
    }
    panic_for(output, "Expected code `{code}`");
}

#[track_caller]
pub fn check_test_err(output: &Output) {
    if !output.status.success() {
        return;
    }
    panic_for(output, "Unexpected successful test")
}

#[track_caller]
pub fn check_test(output: &Output) {
    if output.status.success() {
        return;
    }
    panic_for(output, "Unsuccessful test")
}

#[track_caller]
pub fn panic_for(output: &Output, msg: impl fmt::Display) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    panic!(
        "{msg}: {:?}\
        \nstdout:\
        \n-----\
        \n{stdout}\
        \n-----\
        \nstderr:\
        \n-----\
        \n{stderr}\
        \n-----",
        output.status
    )
}

impl TestConfig {
    pub fn run_lua(&self, lua: &str) -> (Output, Output) {
        let dynamic = self.build_dynamic(lua);
        let dynamic = self.run_dynamic(&dynamic);
        let stat = self.build_static(lua);
        let stat = self.run_static(stat);
        (dynamic, stat)
    }
    pub fn build_static(&self, lua: &str) -> PathBuf {
        let lua_path = self.base_dir.join("main.lua");
        std::fs::write(&lua_path, lua).unwrap();
        let out_path = self.base_dir.join("app");
        let target_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("target")
            .join("llvm");
        std::fs::create_dir_all(&target_dir).ok();
        let file_name = std::thread::current()
            .name()
            .map(ToString::to_string)
            .unwrap_or_else(|| {
                SystemTime::now()
                    .duration_since(SystemTime::UNIX_EPOCH)
                    .unwrap()
                    .as_secs()
                    .to_string()
            });
        let debug_path = target_dir.join(format!("{file_name}.ll"));

        let cmd = Command::new(&self.cmd)
            .arg(&lua_path)
            .arg("-r")
            .arg(&self.static_runtime.parent().unwrap())
            .arg("-o")
            .arg(&debug_path)
            .arg("--filetype")
            .arg("ll")
            .stderr(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .unwrap()
            .wait_with_output()
            .unwrap();
        if !cmd.status.success() {
            eprintln!("Failed to execute debug cmd");
            eprintln!("OUT: {}", String::from_utf8_lossy(&cmd.stdout));
            eprintln!("ERR: {}", String::from_utf8_lossy(&cmd.stderr));
        }
        let cmd = Command::new(&self.cmd)
            .arg(&lua_path)
            .arg("-r")
            .arg(&self.static_runtime.parent().unwrap())
            .arg("-o")
            .arg(&out_path)
            .stderr(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .unwrap()
            .wait_with_output()
            .unwrap();
        if !cmd.status.success() {
            eprintln!("OUT: {}", String::from_utf8_lossy(&cmd.stdout));
            eprintln!("ERR: {}", String::from_utf8_lossy(&cmd.stderr));
            panic!("Failed to execute cmd");
        }
        out_path
    }

    pub fn build_dynamic(&self, lua: &str) -> PathBuf {
        let lua_path = self.base_dir.join("main.lua");
        std::fs::write(&lua_path, lua).unwrap();
        let out_path = self.base_dir.join("app");
        let cmd = Command::new(&self.cmd)
            .env("RUST_LOG", "trace")
            .arg(&lua_path)
            .arg("-r")
            .arg(&self.dynamic_runtime.parent().unwrap())
            .arg("-o")
            .arg(&out_path)
            .stderr(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .unwrap()
            .wait_with_output()
            .unwrap();
        if !cmd.status.success() {
            eprintln!("OUT: {}", String::from_utf8_lossy(&cmd.stdout));
            eprintln!("ERR: {}", String::from_utf8_lossy(&cmd.stderr));
            panic!("Failed to execute cmd");
        }
        out_path
    }

    #[track_caller]
    pub fn run_static(&self, cmd: impl AsRef<Path>) -> Output {
        let mut cmd = Command::new(cmd.as_ref());
        cmd.stderr(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .unwrap()
            .wait_with_output()
            .unwrap()
    }

    #[track_caller]
    pub fn run_dynamic(&self, cmd: impl AsRef<Path>) -> Output {
        let mut cmd = Command::new(cmd.as_ref());
        #[cfg(target_os = "linux")]
        cmd.env("LD_LIBRARY_PATH", self.dynamic_runtime.parent().unwrap());
        #[cfg(target_os = "windows")]
        {
            let path = std::env::var("PATH").expect("PATH is set");
            cmd.env(
                "PATH",
                format!(
                    "{path};{}",
                    self.dynamic_runtime.parent().unwrap().display()
                ),
            );
        }
        #[cfg(target_os = "macos")]
        {
            println!(
                "setting DYLD_FALLBACK_LIBRARY_PATH=`{}`",
                self.dynamic_runtime.parent().unwrap().display()
            );
            cmd.env(
                "DYLD_FALLBACK_LIBRARY_PATH",
                self.dynamic_runtime.parent().unwrap(),
            );
        }
        cmd.stderr(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .unwrap()
            .wait_with_output()
            .unwrap()
    }
}
