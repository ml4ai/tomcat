use futures::StreamExt;
use log::info;
use mqtt::{AsyncReceiver, Message};
use paho_mqtt as mqtt;

pub struct MqttClient {
    pub client: mqtt::AsyncClient,
    client_id: String,
    pub stream: AsyncReceiver<Option<Message>>,
}

impl MqttClient {
    pub fn new(host: &str, port: &u16, client_id: &str) -> Self {
        let address = format!("tcp://{}:{}", host, port);
        // Create the client. Use an ID for a persistent session.
        // A real system should try harder to use a unique ID.
        let create_opts = mqtt::CreateOptionsBuilder::new()
            .mqtt_version(mqtt::MQTT_VERSION_5)
            .server_uri(address)
            .client_id(client_id)
            .finalize();

        // Create the client connection
        let mut client = mqtt::AsyncClient::new(create_opts).unwrap_or_else(|e| {
            panic!("Error creating the client: {e:?}");
        });
        // Get message stream before connecting.
        let mut stream = client.get_stream(25);
        Self {
            client,
            client_id: client_id.to_string(),
            stream,
        }
    }

    pub async fn connect(&mut self) -> Result<(), mqtt::Error> {
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
            &self.client_id
        );

        &self.client.connect(conn_opts).await?;
        Ok::<(), mqtt::Error>(())
    }

    pub async fn subscribe(&self, topics: Vec<String>) -> Result<(), mqtt::Error> {
        let qos = vec![mqtt::QOS_2; topics.len()];

        info!(
            "Subscribing to topics: {:?} with QOS {}",
            topics,
            mqtt::QOS_2
        );

        let sub_opts = vec![mqtt::SubscribeOptions::default(); topics.len()];
        &self
            .client
            .subscribe_many_with_options(topics.as_slice(), qos.as_slice(), &sub_opts, None)
            .await?;
        Ok::<(), mqtt::Error>(())
    }
}
