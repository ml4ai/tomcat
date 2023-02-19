//! File-based reprocessor.

use clap::Parser;
use std::{
    fs::File,
    io::{self, BufRead},
};
use tomcat::{
    get_extractions::get_extractions,
    messages::{chat::ChatMessage, get_message::get_message_from_string, nlu::NLUMessage},
};

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

    let input_file = File::open(&args.input).unwrap();
    let lines = io::BufReader::new(input_file).lines();

    let mut callsign_mapping = std::collections::HashMap::<String, String>::new();

    for line in lines {
        if let Ok(l) = line {
            let value: json::Value = json::from_str(&l).unwrap();
            let topic = value["topic"].as_str();

            if let Some("trial") = topic {
                let sub_type = value["msg"]["sub_type"].as_str();
                if let Some("start") = sub_type {
                    // Construct a mapping between callsigns and participant IDs.
                    for client in value["data"]["client_info"].as_array().unwrap() {
                        callsign_mapping.insert(
                            client["participant_id"].as_str().unwrap().to_string(),
                            client["callsign"].as_str().unwrap().to_string(),
                        );
                    }
                }
            }
            if let Some("communication/chat") = topic {
                let sender = value["data"]["sender_id"].as_str().unwrap();
                let sender = callsign_mapping.get(sender).unwrap().to_string();
                let trial_id = value["msg"]["trial_id"].as_str().unwrap();
                let experiment_id = value["msg"]["experiment_id"].as_str().unwrap();
                let text = &value["data"]["message"].as_str().unwrap();
                let extractions = get_extractions(&text, "http://localhost:8080");
                let message = NLUMessage::new(
                    text.to_string(),
                    sender,
                    extractions,
                    trial_id.to_string(),
                    experiment_id.to_string(),
                );
                let json_serialized_message = json::to_string(&message).unwrap();
                println!("{}", json_serialized_message);
            }
        }
    }
}
