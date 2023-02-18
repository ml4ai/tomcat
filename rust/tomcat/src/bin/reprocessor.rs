//! File-based reprocessor.

use clap::Parser;
use std::{
    fs::File,
    io::{self, BufRead},
};
use tomcat::get_extractions::get_extractions;
use tomcat::messages::{chat::ChatMessage, get_message::get_message_from_string};

use serde_json as json;

/// Command line arguments
#[derive(Parser, Debug)]
pub struct Cli {
    /// Input file
    pub input: String,

    /// Config file
    #[arg(short, long, default_value_t = String::from("config.yml"))]
    pub config: String,
}

fn main() {
    let args = Cli::parse();

    let f = File::open(&args.input).unwrap();
    let lines = io::BufReader::new(f).lines();

    for line in lines {
        if let Ok(l) = line {
            let value: json::Value = json::from_str(&l).unwrap();
            let topic = value["topic"].as_str();
            if let Some("communication/chat") = topic {
                let text = &value["data"]["message"].as_str().unwrap();
                let extractions = get_extractions(&text, "http://localhost:8080");
                println!("{:?}", &extractions);
            }
        }
    }
}
