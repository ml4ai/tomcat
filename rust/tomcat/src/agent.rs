use crate::config::Config;
use crate::mqtt_client::MqttClient;
use crate::{
    messages::internal::{InternalChat, InternalStageTransition},
    messages::{
        chat::{ChatMessage, Extraction},
        stage_transition::{MissionStage, StageTransitionMessage},
        trial::TrialMessage,
    },
    mission_state::MissionState,
};
use futures::{executor, StreamExt};
use log::{error, info, warn};
use paho_mqtt as mqtt;
use serde::Deserialize;
use std::time::Duration;

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

/// Get Odin extractions.
fn get_extractions(text: &str, cfg: &Config) -> Vec<Extraction> {
    let client = reqwest::blocking::Client::new();
    let res = client
        .post(&cfg.event_extractor_url)
        .body(text.to_string())
        .send()
        .unwrap_or_else(|_| {
            panic!(
                "Unable to contact the event extraction backend at {}, is the service running?",
                &cfg.event_extractor_url
            )
        });
    let extractions = serde_json::from_str(&res.text().unwrap()).unwrap();
    extractions
}

/// Process chat message.
fn process_chat_message(msg: mqtt::Message, mission_state: &mut MissionState, cfg: &Config) {
    dbg!(&msg);
    // For ASIST Study 4, only the shop stage will have free text chat.
    if let MissionStage::shop_stage = mission_state.stage {
        let message: ChatMessage = get_message(&msg);
        let sender = message.data.sender;

        if let Some(msg_text) = message.data.text {
            println!(
                "{}",
                serde_json::to_string(&InternalChat {
                    timestamp: message.header.timestamp,
                    sender: mission_state
                        .callsign_mapping
                        .get(&sender.unwrap())
                        .unwrap()
                        .to_string(),
                    text: msg_text.clone()
                })
                .unwrap()
            );
            let extractions = get_extractions(&msg_text, cfg);
            dbg!(&extractions);
        }
    }
}

pub struct Agent {
    mqtt_client: MqttClient,
    config: Config,
}

impl Agent {
    pub fn new(cfg: Config) -> Self {
        let mqtt_client = MqttClient::new(&cfg.mqtt_opts.host, &cfg.mqtt_opts.port, &cfg.client_id);
        let agent = Self {
            mqtt_client,
            config: cfg.clone(),
        };
        agent
    }

    pub fn run(&mut self) -> Result<(), mqtt::Error> {
        let fut_values = async {
            &self.mqtt_client.connect().await;
            &self.mqtt_client.subscribe(self.config.topics.clone()).await;
            // Just loop on incoming messages.
            info!("Waiting for messages...");

            // Note that we are not providing a way to cleanly shut down and
            // disconnect. Therefore, when you kill this app (with a ^C or
            // whatever) the server will get an unexpected drop and then
            // should emit the LWT message.

            let mut mission_state = MissionState::default();

            while let Some(msg_opt) = self.mqtt_client.stream.next().await {
                if let Some(msg) = msg_opt {
                    if msg.retained() {
                        print!("(R) ");
                    }
                    match msg.topic() {
                        "communication/chat" => {
                            process_chat_message(msg, &mut mission_state, &self.config)
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
                    while let Err(err) = &self.mqtt_client.client.reconnect().await {
                        error!("Error reconnecting: {}", err);

                        // For tokio use: tokio::time::delay_for()
                        async_std::task::sleep(Duration::from_millis(1000)).await;
                    }
                }
            }

            // Explicit return type for the async block
            Ok::<(), mqtt::Error>(())
        };

        let values = executor::block_on(fut_values);
        //if let Err(err) = values {
        //eprintln!("{err}");
        //}
        values
    }
}
