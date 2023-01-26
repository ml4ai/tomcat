//! ToMCAT NLU agent

use clap::Parser;
use futures::{executor::block_on, StreamExt};
use paho_mqtt as mqtt;
use serde::{Deserialize, Serialize};
use std::time::Duration;
use tomcat::messages::internal::{InternalChat, InternalStageTransition};
use tomcat::mission_state::MissionState;
use tomcat::{
    cli::Cli,
    messages::{
        chat::{ChatMessage, Extraction},
        stage_transition::{MissionStage, StageTransitionMessage},
        trial::TrialMessage,
    },
};

use ispell::{SpellChecker, SpellLauncher};
use log::{error, info, warn};

/// Configuration
#[derive(Default, Debug, Serialize, Deserialize)]
struct Config {
    topics: Vec<String>,
    client_id: String,
    event_extractor_url: String,
    custom_vocabulary: Vec<String>,
}

/// Deserialize message to a struct.
fn get_message<'a, T: Deserialize<'a>>(message: &'a mqtt::Message) -> T {
    serde_json::from_slice::<T>(message.payload()).unwrap_or_else(|_| {
        panic!(
            "Unable to deserialize JSON payload {:?} for message arriving on topic '{}'",
            message.payload_str(),
            message.topic(),
        )
    })
}

/// Process stage transition messages.
fn process_stage_transition_message(message: mqtt::Message, mission_state: &mut MissionState) {
    let message: StageTransitionMessage = get_message(&message);
    println!(
        "{}",
        serde_json::to_string(&InternalStageTransition {
            timestamp: message.header.timestamp,
            stage: message.data.mission_stage.to_string(),
        })
        .unwrap()
    );
    mission_state.stage = message.data.mission_stage;
}

/// Process trial messages.
fn process_trial_message(message: mqtt::Message, mission_state: &mut MissionState) {
    let message: TrialMessage = get_message(&message);
    if let "start" = message.msg.sub_type.as_str() {
        // Construct a mapping between callsigns and participant IDs.
        for client in message.data.client_info {
            mission_state
                .callsign_mapping
                .insert(client.participant_id.unwrap(), client.callsign.unwrap());
        }
    }
}

// Maybe translate the Extractions vector as a serde_json Array Value?
fn do_something_with_extractions(extractions: Vec<Extraction>) {
    println!("Extractions: {:#?}", extractions);
}

fn check_spelling(text: &str, checker: &mut SpellChecker, cfg: &Config) {
    let errors = checker.check(text).expect("Unable to check spelling!");
    for e in errors {
        // Only show errors if the misspelled word is not in the custom vocabulary.
        if !cfg.custom_vocabulary.contains(&e.misspelled) {
            println!("'{}' (pos: {}) is misspelled!", &e.misspelled, e.position);
            if !e.suggestions.is_empty() {
                println!("Maybe you meant '{}'?", &e.suggestions[0]);
            }
        }
    }
}

/// Get Odin extractions.
fn get_extractions(text: String, cfg: &Config) {
    let client = reqwest::blocking::Client::new();
    let res = client
        .post(&cfg.event_extractor_url)
        .body(text)
        .send()
        .unwrap_or_else(|_| {
            panic!(
                "Unable to contact the event extraction backend at {}, is the service running?",
                &cfg.event_extractor_url
            )
        });
    let vec: Vec<Extraction> = serde_json::from_str(&res.text().unwrap()).unwrap();
}

/// Process chat message.
fn process_chat_message(
    msg: mqtt::Message,
    mission_state: &mut MissionState,
    checker: &mut SpellChecker,
    cfg: &Config,
) {
    // For ASIST Study 4, only the shop stage will have free text chat.
    if let MissionStage::shop_stage = mission_state.stage {
        let message: ChatMessage = get_message(&msg);
        let sender = message.data.sender;

        if let Some(msg_text) = message.data.text {
            //println!(
            //"[{}] {}: {}",
            //message.header.timestamp,
            //mission_state.callsign_mapping.get(&sender.unwrap()).unwrap(),
            //msg_text
            //);

            println!(
                "{}",
                serde_json::to_string(&InternalChat {
                    timestamp: message.header.timestamp,
                    sender: mission_state
                        .callsign_mapping
                        .get(&sender.unwrap())
                        .unwrap()
                        .to_string(),
                    text: msg_text
                })
                .unwrap()
            );
            //check_spelling(&msg_text, checker, cfg);
            //get_extractions(msg_text, cfg);
        }
    }
}

fn main() {
    // Initialize the logger from the environment
    pretty_env_logger::init();

    let args = Cli::parse();

    let cfg: Config = confy::load_path(&args.config)
        .unwrap_or_else(|_| panic!("Unable to load config file {}!", &args.config));
    let address = format!("tcp://{}:{}", &args.host, &args.port);

    // Create the client. Use an ID for a persistent session.
    // A real system should try harder to use a unique ID.
    let create_opts = mqtt::CreateOptionsBuilder::new()
        .mqtt_version(mqtt::MQTT_VERSION_5)
        .server_uri(address)
        .client_id(&cfg.client_id)
        .finalize();

    // Create the client connection
    let mut client = mqtt::AsyncClient::new(create_opts).unwrap_or_else(|e| {
        panic!("Error creating the client: {:?}", e);
    });

    if let Err(err) = block_on(async {
        // Get message stream before connecting.
        let mut strm = client.get_stream(25);

        // Define the set of options for the connection
        let lwt = mqtt::Message::new("test", "Async subscriber lost connection", mqtt::QOS_1);

        let conn_opts = mqtt::ConnectOptionsBuilder::new()
            .mqtt_version(mqtt::MQTT_VERSION_5)
            .clean_start(true)
            .properties(mqtt::properties![mqtt::PropertyCode::SessionExpiryInterval => 3600])
            .will_message(lwt)
            .finalize();

        // Make the connection to the broker
        info!(
            "Connecting to the MQTT server with client id \"{}\"",
            &cfg.client_id
        );
        client.connect(conn_opts).await?;

        let qos = vec![mqtt::QOS_2; cfg.topics.len()];

        info!(
            "Subscribing to topics: {:?} with QOS {}",
            cfg.topics,
            mqtt::QOS_2
        );
        let sub_opts = vec![mqtt::SubscribeOptions::default(); cfg.topics.len()];
        client.subscribe_many_with_options(cfg.topics.as_slice(), qos.as_slice(), &sub_opts, None)
            .await?;

        // Just loop on incoming messages.
        info!("Waiting for messages...");

        // Note that we are not providing a way to cleanly shut down and
        // disconnect. Therefore, when you kill this app (with a ^C or
        // whatever) the server will get an unexpected drop and then
        // should emit the LWT message.

        let mut mission_state = MissionState::default();

        let mut checker = SpellLauncher::new()
            .aspell()
            .dictionary("en_US")
            .launch()
            .unwrap();

        while let Some(msg_opt) = strm.next().await {
            if let Some(msg) = msg_opt {
                if msg.retained() {
                    print!("(R) ");
                }
                match msg.topic() {
                    "communication/chat" => {
                        process_chat_message(msg, &mut mission_state, &mut checker, &cfg)
                    }
                    "observations/events/stage_transition" => {
                        process_stage_transition_message(msg, &mut mission_state)
                    }
                    "trial" => process_trial_message(msg, &mut mission_state),
                    _ => {
                        warn!("Unhandled topic: {}", msg.topic());
                    }
                }
            } else {
                // A "None" means we were disconnected. Try to reconnect...
                info!("Lost connection. Attempting reconnect.");
                while let Err(err) = client.reconnect().await {
                    error!("Error reconnecting: {}", err);

                    // For tokio use: tokio::time::delay_for()
                    async_std::task::sleep(Duration::from_millis(1000)).await;
                }
            }
        }

        // Explicit return type for the async block
        Ok::<(), mqtt::Error>(())
    }) {
        eprintln!("{}", err);
    }
}
