use clap::Parser;

/// Command line arguments
#[derive(Parser, Debug)]
pub struct Cli {
    /// MQTT broker host
    #[arg(long, default_value_t = String::from("localhost"))]
    pub host: String,

    /// MQTT broker port
    #[arg(short, long, default_value_t = 1883)]
    pub port: u16,

    /// Config file
    #[arg(short, long, default_value_t = String::from("config.yml"))]
    pub config: String,
}
