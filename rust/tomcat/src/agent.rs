use crate::{
    config::Config,
    mqtt_client::MqttClient,
    get_extractions::get_extractions,
    knowledge_base::KnowledgeBase,
    messages::{
        chat::ChatMessage,
        get_message::get_message,
        nlu::NLUMessage,
        stage_transition::StageTransitionMessage,
        trial::TrialMessage,
    },
};
use futures::{executor, StreamExt};
use log::{error, info, warn};
use paho_mqtt as mqtt;
use std::time::Duration;

pub struct Agent {
    mqtt_client: MqttClient,
    config: Config,
    kb: KnowledgeBase,
}

impl Agent {
    /// Construct a new Agent struct.
    pub fn new(cfg: Config) -> Self {
        let mqtt_client = MqttClient::new(&cfg.mqtt.host, &cfg.mqtt.port, &cfg.client_id);
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

            let compact_message = NLUMessage::new(
                msg_text,
                sender,
                extractions,
                message.msg.trial_id.clone(),
                message.msg.trial_id.clone(),
            );

            let publish_topic = "agent".to_owned() + self.config.client_id.as_str();

            let message = mqtt::Message::new(
                publish_topic,
                serde_json::to_string(&compact_message).unwrap(),
                2,
            );
            self.publish(message);
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
            let _ = &self
                .mqtt_client
                .subscribe(self.config.subscribe_topics.clone())
                .await;
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
