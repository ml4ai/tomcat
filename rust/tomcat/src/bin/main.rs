//! ToMCAT NLU agent

use clap::Parser;
use tomcat::agent::Agent;
use tomcat::config::Config;

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

fn main() {
    // Initialize the logger from the environment
    pretty_env_logger::init();

    let args = Cli::parse();

    let cfg: Config = confy::load_path(&args.config)
        .unwrap_or_else(|_| panic!("Unable to load config file {}!", &args.config));

    let mut agent = Agent::new(cfg);
    agent.run().unwrap();
}
