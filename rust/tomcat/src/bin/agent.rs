//! ToMCAT NLU agent

use clap::Parser;
use tomcat::agent::Agent;
use tomcat::config::Config;

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

fn main() {
    // Initialize the logger from the environment
    pretty_env_logger::init();

    let args = Cli::parse();

    let mut cfg: Config = confy::load_path(&args.config)
        .unwrap_or_else(|_| panic!("Unable to load config file {}!", &args.config));

    // allow user command line args to override config file settings
    if let Some(host) = args.host {
        cfg.mqtt_opts.host = host;
    }
    if let Some(port) = args.port {
        cfg.mqtt_opts.port = port;
    }

    println!(
        "MQTT host = {}, port = {}",
        cfg.mqtt_opts.host, cfg.mqtt_opts.port
    );

    let mut agent = Agent::new(cfg);
    agent.run().unwrap();
}
