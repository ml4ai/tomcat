//! This application is an MQTT subscriber using the asynchronous client
//! interface of the Paho Rust client library.
//! It also monitors for disconnects and performs manual re-connections.
//!
//! The sample demonstrates:
//!   - An async/await subscriber
//!   - Connecting to an MQTT server/broker.
//!   - Subscribing to multiple topics
//!   - Using MQTT v5 subscribe options
//!   - Receiving messages from an async stream.
//!   - Handling disconnects and attempting manual reconnects.
//!   - Using a "persistent" (non-clean) session so the broker keeps
//!     subscriptions and messages through reconnects.
//!   - Last will and testament
//!

use std::time::Duration;
use futures::StreamExt;
use futures::executor::block_on;
use paho_mqtt as mqtt;
use serde::{Serialize, Deserialize};
use clap::Parser;
use pretty_env_logger;
use log::{info, warn, error};

/// Configuration
#[derive(Default, Debug, Serialize, Deserialize)]
struct Config {
    topics: Vec<String>,
    client_id: String
}

/// Command line arguments
#[derive(Parser, Debug)]
struct Cli {
    /// MQTT broker host
    #[arg(long, default_value_t = String::from("localhost"))]
    host: String,

    /// MQTT broker port
    #[arg(short, long, default_value_t = 1883)]
    port: u16,

    /// Config file
    #[arg(short, long, default_value_t = String::from("config.yml"))]
    config: String,
}

#[derive(Debug, Serialize, Deserialize)]
struct Header {
    timestamp: String,
    version: String,
    message_type: String
}

#[derive(Debug, Serialize, Deserialize)]
struct Msg {
    trial_id: String,
    experiment_id: String,
    timestamp: String,
    source: String,
    version: String,
    sub_type: String,
    replay_parent_id: String,
    replay_id: String,
    replay_parent_type: String,
}

#[derive(Debug, Serialize, Deserialize)]
struct ChatData {
    text: String,
    addressees: Vec<String>,
    elapsed_milliseconds: usize,
    mission_timer: String,
    sender: String,
}

fn process_message(msg: mqtt::Message) {
    println!("{}", msg);
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
        info!("Connecting to the MQTT server with client id \"{}\"", &cfg.client_id);
        cli.connect(conn_opts).await?;

        let qos = vec![mqtt::QOS_2; cfg.topics.len()];

        info!("Subscribing to topics: {:?} with QOS {}", cfg.topics, mqtt::QOS_2);
        let sub_opts = vec![mqtt::SubscribeOptions::default(); cfg.topics.len()];
        cli.subscribe_many_with_options(cfg.topics.as_slice(), qos.as_slice(), &sub_opts, None)
            .await?;

        // Just loop on incoming messages.
        info!("Waiting for messages...");

        // Note that we're not providing a way to cleanly shut down and
        // disconnect. Therefore, when you kill this app (with a ^C or
        // whatever) the server will get an unexpected drop and then
        // should emit the LWT message.

        while let Some(msg_opt) = strm.next().await {
            if let Some(msg) = msg_opt {
                if msg.retained() {
                    print!("(R) ");
                }
                match msg.topic() {
                    "minecraft/chat" => process_message(msg),
                    _ => {
                        warn!("Unhandled topic: {}", msg.topic());
                    }
                }
            }
            else {
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
