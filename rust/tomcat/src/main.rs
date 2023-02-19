//! ToMCAT NLU agent

use clap::Parser;
use tomcat::agent::Agent;
use tomcat::cli::Cli;
use tomcat::config::Config;


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

    println!("MQTT host = {}, port = {}", cfg.mqtt_opts.host, cfg.mqtt_opts.port);

    let mut agent = Agent::new(cfg);
    agent.run().unwrap();
}
