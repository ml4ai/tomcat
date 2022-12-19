//! ToMCAT NLU agent

use futures::executor::block_on;
use futures::StreamExt;
use paho_mqtt as mqtt;
use serde::{Deserialize, Serialize};
use std::time::Duration;
use tomcat::messages::{
    chat::{ChatMessage, Extraction},
    stage_transition::{MissionStage, StageTransitionMessage},
};
use tomcat::cli::Cli;
use clap::Parser;
use reqwest;

use ispell::{SpellChecker, SpellLauncher};
use log::{error, info, warn};

/// Configuration
#[derive(Default, Debug, Serialize, Deserialize)]
struct Config {
    topics: Vec<String>,
    client_id: String,
}


fn get_message<'a, T: Deserialize<'a>>(message: &'a mqtt::Message) -> T {
    serde_json::from_slice::<T>(message.payload())
        .unwrap_or_else(|_| panic!("Unable to parse JSON payload {:?}", message.payload()))
}

fn process_stage_transition_message(message: mqtt::Message, mission_stage: &mut MissionStage) {
    let message: StageTransitionMessage = get_message(&message);
    *mission_stage = message.data.mission_stage;
}

fn process_chat_message(
    msg: mqtt::Message,
    mission_stage: &mut MissionStage,
    checker: &mut SpellChecker,
) {
    println!("process_chat_message start");
    // For ASIST Study 4, only the shop stage will have free text chat.
    if let MissionStage::shop_stage = mission_stage {
        let message: ChatMessage = get_message(&msg);

        // We ignore server-generated messages.
        if let "Server" = message.data.sender.as_str() {
        } else {
            let errors = checker.check(&message.data.text).unwrap();
            for e in errors {
                println!("'{}' (pos: {}) is misspelled!", &e.misspelled, e.position);
                if !e.suggestions.is_empty() {
                    println!("Maybe you meant '{}'?", &e.suggestions[0]);
                }
            }
            println!("{}", &message.data.text);
            let client = reqwest::blocking::Client::new();
            let res = client.post("http://localhost:8080").body(message.data.text).send().unwrap();
            let text = res.text().unwrap();

            let foo: Vec<Extraction> = serde_json::from_str(&text).unwrap(); 
            println!("{:#?}", &foo);
        }
        
    }
    println!("process_chat_message done");
}

fn main() {
    // Initialize the logger from the environment
    pretty_env_logger::init();

    let args = Cli::parse();

    let cfg: Config = confy::load_path(&args.config).unwrap();
    let address = format!("tcp://{}:{}", &args.host, &args.port);

    // Create the client. Use an ID for a persistent session.
    // A real system should try harder to use a unique ID.
    let create_opts = mqtt::CreateOptionsBuilder::new()
        .mqtt_version(mqtt::MQTT_VERSION_5)
        .server_uri(address)
        .client_id(&cfg.client_id)
        .finalize();

    // Create the client connection
    let mut cli = mqtt::AsyncClient::new(create_opts).unwrap_or_else(|e| {
        panic!("Error creating the client: {:?}", e);
    });

    if let Err(err) = block_on(async {
        // Get message stream before connecting.
        let mut strm = cli.get_stream(25);

        // Define the set of options for the connection
        let lwt = mqtt::Message::new("test", "Async subscriber lost connection", mqtt::QOS_1);

        let conn_opts = mqtt::ConnectOptionsBuilder::new()
            .mqtt_version(mqtt::MQTT_VERSION_5)
            .clean_start(false)
            .properties(mqtt::properties![mqtt::PropertyCode::SessionExpiryInterval => 3600])
            .will_message(lwt)
            .finalize();

        // Make the connection to the broker
        info!(
            "Connecting to the MQTT server with client id \"{}\"",
            &cfg.client_id
        );
        cli.connect(conn_opts).await?;

        let qos = vec![mqtt::QOS_2; cfg.topics.len()];

        info!(
            "Subscribing to topics: {:?} with QOS {}",
            cfg.topics,
            mqtt::QOS_2
        );
        let sub_opts = vec![mqtt::SubscribeOptions::default(); cfg.topics.len()];
        cli.subscribe_many_with_options(cfg.topics.as_slice(), qos.as_slice(), &sub_opts, None)
            .await?;

        // Just loop on incoming messages.
        info!("Waiting for messages...");

        // Note that we are not providing a way to cleanly shut down and
        // disconnect. Therefore, when you kill this app (with a ^C or
        // whatever) the server will get an unexpected drop and then
        // should emit the LWT message.

        let mut mission_stage = MissionStage::shop_stage;

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
                    "minecraft/chat" => process_chat_message(msg, &mut mission_stage, &mut checker),
                    "observations/events/stage_transition" => {
                        process_stage_transition_message(msg, &mut mission_stage)
                    }
                    _ => {
                        warn!("Unhandled topic: {}", msg.topic());
                    }
                }
            } else {
                // A "None" means we were disconnected. Try to reconnect...
                info!("Lost connection. Attempting reconnect.");
                while let Err(err) = cli.reconnect().await {
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
