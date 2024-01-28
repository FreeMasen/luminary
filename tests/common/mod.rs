use std::{path::{Path, PathBuf}, process::{Command, Output, Stdio}};

use escargot::format::Message;

const TEMP_DIR: &str = env!("CARGO_TARGET_TMPDIR");
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
    let dynamic_runtime = dynamic_path.expect("Didn't generate a dynamic runtime lib");
    let static_runtime = static_path.expect("Didn't generate a static runtime");
    let base_dir = PathBuf::from(TEMP_DIR).join(name);
    let slib = base_dir.join("slib");
    let _ = std::fs::create_dir_all(&slib);
    let dynamic_dest = slib.join(dynamic_runtime.file_name().unwrap());
    std::fs::copy(dynamic_runtime, &dynamic_dest).unwrap();
    let lib = base_dir.join("lib");
    let _ = std::fs::create_dir_all(&lib);
    let static_dest = lib.join(static_runtime.file_name().unwrap());
    std::fs::copy(static_runtime, &static_dest).unwrap();

    TestConfig {
        dynamic_runtime: dynamic_dest,
        static_runtime: static_dest,
        cmd: PathBuf::from(env!("CARGO_BIN_EXE_luminary")),
        base_dir,
    }
}

impl TestConfig {
    pub fn build_static(&self, lua: &str) -> PathBuf {
        let lua_path = self.base_dir.join("main.lua");
        std::fs::write(&lua_path, lua).unwrap();
        let out_path = self.base_dir.join("app");
        let cmd = Command::new(&self.cmd)
            .arg(&lua_path)
            .arg("-r")
            .arg(&self.static_runtime.parent().unwrap())
            .arg("-o")
            .arg(&out_path)
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

    pub fn run_dynamic(&self, cmd: impl AsRef<Path>) -> Output {
        let mut cmd = Command::new(cmd.as_ref());
        #[cfg(target_os = "linux")]
        cmd.env("LD_LIBRARY_PATH", self.dynamic_runtime.parent().unwrap());
        #[cfg(target_os = "windows")]
        {
            let path = std::env::var("PATH");
            cmd.env("PATH", format!("{path};{}", self.dynamic_runtime.parent().unwrap()));
        }
        #[cfg(target_os = "macos")]
        cmd.env("DYLD_FALLBACK_LIBRARY_PATH", self.dynamic_runtime.parent().unwrap());

        cmd.stderr(Stdio::inherit())
            .stdout(Stdio::piped())
            .spawn()
            .unwrap()
            .wait_with_output()
            .unwrap()
    }
}
