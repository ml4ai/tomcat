use clap::Parser;

/// Command line arguments
#[derive(Parser, Debug)]
pub struct Cli {
    /// MQTT broker host
    #[arg(long)]
    pub host: Option<String>,

    /// MQTT broker port
    #[arg(short, long)]
    pub port: Option<u16>,

    /// Config file
    #[arg(short, long, default_value_t = String::from("config.yml"))]
    pub config: String,
}
