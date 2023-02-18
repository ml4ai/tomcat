use crate::config::Config;
use crate::mqtt_client::MqttClient;
use crate::{
    knowledge_base::KnowledgeBase,
    get_extractions::get_extractions,
    messages::{
        get_message::get_message,
        internal::InternalStageTransition,
        chat::{ChatMessage, Extraction},
        common::{Header, Msg},
        nlu::{CompactData, CompactMessage},
        stage_transition::{MissionStage, StageTransitionMessage},
        trial::TrialMessage,
    },
};
use futures::{executor, StreamExt};
use iso8601_timestamp::Timestamp;
use log::{error, info, warn};
use paho_mqtt as mqtt;
use serde::Deserialize;
use serde_json::Value;
use std::time::Duration;


pub struct Agent {
    mqtt_client: MqttClient,
    config: Config,
    kb: KnowledgeBase,
}

impl Agent {
    /// Construct a new Agent struct.
    pub fn new(cfg: Config) -> Self {
        let mqtt_client = MqttClient::new(&cfg.mqtt_opts.host, &cfg.mqtt_opts.port, &cfg.client_id);
        let kb = KnowledgeBase::default();
        let agent = Self {
            mqtt_client,
            config: cfg.clone(),
            kb,
        };
        agent
    }


    /// Process chat message.
    fn process_chat_message(&self, msg: mqtt::Message) {
        // For ASIST Study 4, only the shop stage will have free text chat.
        if let MissionStage::shop_stage = &self.kb.stage {
            let message: ChatMessage = get_message(&msg);
            let sender = message.data.sender;

            if let Some(msg_text) = message.data.text {
                let sender = self
                    .kb
                    .callsign_mapping
                    .get(&sender.unwrap())
                    .unwrap()
                    .to_string();

                let extractions = get_extractions(&msg_text, &self.config.event_extractor_url);

                let header = Header {
                    timestamp: Timestamp::now_utc(),
                    version: "1.2".to_string(),
                    message_type: "agent".to_string(),
                };

                let msg_1 = Msg {
                    trial_id: self.kb.trial_id.clone(),
                    experiment_id: self.kb.experiment_id.clone(),
                    //timestamp: Timestamp::now_utc(),
                    source: "".to_string(),
                    version: "0.1".to_string(),
                    sub_type: "".to_string(),
                    replay_parent_id: None,
                    replay_id: None,
                    replay_parent_type: None,
                };

                let compact_data = CompactData {
                    participant_id: sender,
                    extractions,
                };

                let compact_message = CompactMessage {
                    header,
                    msg: msg_1,
                    data: compact_data,
                };

                let message = mqtt::Message::new(
                    self.config.publish_topic.clone(),
                    serde_json::to_string(&compact_message).unwrap(),
                    2,
                );
                self.publish(message);
            }
        }
    }

    fn publish(&self, msg: mqtt::Message) {
        self.mqtt_client.client.publish(msg);
    }

    /// Process stage transition messages.
    fn process_stage_transition_message(&mut self, message: mqtt::Message) {
        let message: StageTransitionMessage = get_message(&message);
        self.kb.stage = message.data.mission_stage;
    }

    /// Process trial messages.
    fn process_trial_message(&mut self, message: mqtt::Message) {
        let message: TrialMessage = get_message(&message);
        if let "start" = message.msg.sub_type.as_str() {
            // Construct a mapping between callsigns and participant IDs.
            for client in message.data.client_info {
                self.kb
                    .callsign_mapping
                    .insert(client.participant_id.unwrap(), client.callsign.unwrap());
            }

            // Update trial ID
            self.kb.trial_id = message.msg.trial_id;

            // Update experiment ID
            self.kb.experiment_id = message.msg.experiment_id;
        }
    }

    fn process_message(&mut self, msg: mqtt::Message) {
        match msg.topic() {
            "communication/chat" => self.process_chat_message(msg),
            "observations/events/stage_transition" => self.process_stage_transition_message(msg),
            "trial" => self.process_trial_message(msg),
            _ => {
                warn!("Unhandled topic: {}", msg.topic());
            }
        }
    }

    pub fn run(&mut self) -> Result<(), mqtt::Error> {
        let fut_values = async {
            let _ = &self.mqtt_client.connect().await;
            let _ = &self.mqtt_client.subscribe(self.config.subscribe_topics.clone()).await;
            // Just loop on incoming messages.
            info!("Waiting for messages...");

            // Note that we are not providing a way to cleanly shut down and
            // disconnect. Therefore, when you kill this app (with a ^C or
            // whatever) the server will get an unexpected drop and then
            // should emit the LWT message.

            while let Some(msg_opt) = self.mqtt_client.stream.next().await {
                if let Some(msg) = msg_opt {
                    self.process_message(msg);
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
