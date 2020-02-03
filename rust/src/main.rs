use std::env;
use std::fs::File;
use std::process::exit;
use std::path::Path;
use std::io::Write;

use log::{Record, Level, Metadata, LevelFilter, info};

mod filewall;

static CONSOLE_LOGGER: ConsoleLogger = ConsoleLogger; 

struct ConsoleLogger;

impl log::Log for ConsoleLogger {
  fn enabled(&self, metadata: &Metadata) -> bool {
     metadata.level() <= Level::Info
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            println!("{} - {}", record.level(), record.args());
        }
    }

    fn flush(&self) {}
}

fn print_help() {
    println!("filewall.py -v <apikey> <source_file> <replace_source>");
    println!("    -v            : verbose (optional)");
    println!("    apikey        : your apikey");
    println!("    source_file   : local file");
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.contains(&String::from("help")) || args.len() < 3 {
        print_help();
        exit(1);
    }

    if args.contains(&String::from("-v")) {
        log::set_logger(&CONSOLE_LOGGER).unwrap();
        log::set_max_level(LevelFilter::Info);
    }

    let n = args.len();
    let apikey = &args[n - 2];
    let source_file = &args[n - 1];

    let file = File::open(&source_file).unwrap();
    let source_path = Path::new(&source_file);
    let source_name = source_path.file_name().unwrap().to_str().unwrap();
    let result = filewall::FileWall::new(&apikey).convert(source_name, file);

    if result.is_err() {
        println!("Error: {}", result.unwrap_err());
        exit(1);
    }

    let result = result.unwrap();
    let path = Path::new(&result.0);

    let stem = String::from(path.file_stem().unwrap().to_str().unwrap());
    let extension = path.extension().unwrap().to_str().unwrap();

    let mut unique = stem.clone() + "." + extension;
    let mut i = 0;
    while Path::new(&unique).exists() {
        i += 1;
        unique = stem.clone() + "_" + &i.to_string() + "." + extension;
    }
    let mut pdf_file = File::create(&unique).unwrap();
    pdf_file.write(&result.1).unwrap();

    info!("Secure result: {}", unique)
}
