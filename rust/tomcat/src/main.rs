//! ToMCAT NLU agent

use clap::Parser;
use tomcat::agent::Agent;
use tomcat::cli::Cli;
use tomcat::config::{Config, MqttOpts};


fn main() {
    // Initialize the logger from the environment
    pretty_env_logger::init();

    let args = Cli::parse();

    let mut cfg: Config = confy::load_path(&args.config)
        .unwrap_or_else(|_| panic!("Unable to load config file {}!", &args.config));

    // allow user command line args to override config file settings.
    cfg.mqtt_opts = MqttOpts{host: args.host, port: args.port};

    let mut agent = Agent::new(cfg);
    agent.run().unwrap();
}
