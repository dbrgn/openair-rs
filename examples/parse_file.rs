use std::fs::File;
use std::io::BufReader;

use env_logger;

use skytraxx_airspace_parser::parse;

macro_rules! fail {
    ($msg:expr) => {{
        println!("Error: {}", $msg);
        std::process::exit(1);
    }}
}

fn main() -> std::io::Result<()> {
    env_logger::init();

    println!("Starting parser...");

    // Open file
    let filename = match std::env::args().nth(1) {
        Some(f) => f,
        None => fail!("Missing filename parameter"),
    };
    let file = File::open(filename)?;
    let mut reader = BufReader::new(file);

    // Process airspaces
    println!("Airspaces:");
    while let Some(airspace) = parse(&mut reader).unwrap_or_else(|e| fail!(e)) {
        println!("- {}", airspace);
    }
    println!("Done.");

    Ok(())
}
