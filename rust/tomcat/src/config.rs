use serde::{Deserialize, Serialize};

#[derive(Default, Debug, Serialize, Deserialize, Clone)]
pub struct MqttOpts {
    pub host: String,
    pub port: u16,
}

/// Configuration
#[derive(Default, Debug, Serialize, Deserialize, Clone)]
pub struct Config {
    pub topics: Vec<String>,
    pub client_id: String,
    pub mqtt_opts: MqttOpts,
    pub event_extractor_url: String,
    pub custom_vocabulary: Vec<String>,
}
