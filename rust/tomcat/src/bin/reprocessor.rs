//! File-based reprocessor.

use clap::Parser;
use std::{
    collections::HashSet,
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
            // We construct sets of trial IDs to handle special cases arising due to changing data
            // formats during the ASIST Study 4 pilot runs.
            let trial_set_1 = HashSet::from([
                "b1532325-3869-4265-b69a-a601ad4da842",
                "616e855d-b48c-4985-9229-69432f45a3cc",
                "a29ee710-e6c4-4791-a21c-f85a6b8d6087",
                "e8ac5a06-ef96-4a70-82d9-2591d3aaa84d",
                "432f5af6-9163-4c0e-a75b-fdd931ba1430",
                "b6971d21-f810-4f16-bb63-1a9286de8d9d",
                "2554b670-dcb3-4cca-b336-0871595c8afb",
                "082f4e1f-245a-4160-b1fc-ef01f1107a3e",
                "ad596a70-510d-43d9-8162-1fc3a03ef58b",
                "f5cfa219-bdaf-4548-95a4-0d9474e2e9f9",
            ]);

            // In this set of trials:
            // - The chat topic is minecraft/chat instead of communication/chat.
            // - The .data.sender key contains the callsign instead of the participant ID, and is
            //   sometimes 'Server'.
            let trial_set_2 = HashSet::from([
                "f5cfa219-bdaf-4548-95a4-0d9474e2e9f9",
                "56e661e3-cbaa-48e9-9444-705647f934de",
            ]);

            let value: json::Value = json::from_str(&l).unwrap();
            let trial_id = value["msg"]["trial_id"].as_str().unwrap();
            let chat_topic: &str;

            if trial_set_2.contains(trial_id) {
                chat_topic = "minecraft/chat";
            } else {
                chat_topic = "communication/chat";
            }

            let topic = value["topic"].as_str();

            if topic == Some("trial") {
                let sub_type = value["msg"]["sub_type"].as_str();
                if let Some("start") = sub_type {
                    // Construct a mapping between callsigns and participant IDs.
                    for client in value["data"]["client_info"].as_array().unwrap() {
                        if trial_set_2.contains(trial_id) {
                            // For trials in trial_set_2, the playername is published in the
                            // .data.sender field sometimes. We construct this callsign mapping to
                            // help normalize the data to consistently use the callsign.
                            callsign_mapping.insert(
                                client["playername"].as_str().unwrap().to_string(),
                                client["callsign"].as_str().unwrap().to_string(),
                            );
                        } else {
                            callsign_mapping.insert(
                                client["participant_id"].as_str().unwrap().to_string(),
                                client["callsign"].as_str().unwrap().to_string(),
                            );
                        }
                    }
                }
            }

            let mut sender: &str;
            let text: &str;
            if topic == Some(chat_topic) {
                if trial_set_1.contains(trial_id) || trial_set_2.contains(trial_id) {
                    sender = value["data"]["sender"].as_str().unwrap();
                    text = value["data"]["text"].as_str().unwrap();

                    // For trials in trial_set_2, the .data.sender field is set to 'Server' for
                    // messages originating from the testbed rather than from players. We ignore
                    // these as they are not natural language utterances.
                    if trial_set_2.contains(trial_id) {
                        if sender == "Server" {
                            continue;
                        }
                    }
                } else {
                    sender = value["data"]["sender_id"].as_str().unwrap();
                    text = value["data"]["message"].as_str().unwrap();
                }

                // For trials in trial_set_2, the sender field usually contains the callsign, so we don't
                // have to look up the callsign in the callsign_mapping struct.
                if !trial_set_2.contains(trial_id) {
                    sender = callsign_mapping.get(sender).unwrap();
                } else {
                    // For trials in trial_set_2, the sender field sometimes contains the
                    // playername instead of the callsign (I don't know why). So we look up the
                    // playername in the callsign mapping to normalize the data.
                    if let Some(s) = callsign_mapping.get(sender) {
                        sender = s;
                    }
                }

                let experiment_id = value["msg"]["experiment_id"].as_str().unwrap();
                let extractions = get_extractions(text, "http://localhost:8080");
                let message = NLUMessage::new(
                    text.to_string(),
                    // In the earlier pilot runs, the callsigns are fully uppercased (e.g., RED).
                    // We convert them to lowercase for consistency with the later trials.
                    sender.to_string().to_lowercase(),
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
