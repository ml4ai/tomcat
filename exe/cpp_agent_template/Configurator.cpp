#include "Configurator.hpp"

#include <boost/filesystem.hpp>
#include <boost/iostreams/device/file.hpp>
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <boost/program_options.hpp>
#include <stdlib.h>
#include <iostream>
#include <exception>

namespace po = boost::program_options;
namespace fs = boost::filesystem;
namespace json = boost::json;

json::object Configurator::parse_args(int argc, char* argv[]) {

    // set up options
    po::options_description desc = describe_options();

    // parse command line args into options
    po::variables_map vm;
    try {
	po::parsed_options parsed = po::command_line_parser(argc, argv)
            .options(desc)
	    .allow_unregistered()
	    .run();
        po::store(parsed, vm);
        po::notify(vm);

	// don't run if there are unrecognized args
        std::vector<std::string> unknown_args = 
            po::collect_unrecognized(parsed.options,po::include_positional);

	for(auto &wut : unknown_args) {
	    std::cout << "Unknown arg: " << wut << std::endl;
            exit(EXIT_FAILURE);
	}

    } catch (std::exception& e) {
        std::cerr << "Could not parse command line args:" << std::endl;
	std::cerr << "Exception: " << e.what() << std::endl;
        exit(EXIT_FAILURE);
    }


    // if the user wants the help page, show it and exit
    if (vm.count("help")) {
        std::cout << desc << std::endl;
        exit(EXIT_SUCCESS);
    }

    // parse the config file as JSON
    std::string config_filename = vm["config"].as<std::string>();
    json::object config = parse_config_file(config_filename);

    // add or override user specified MQTT fields.
    json::value mqtt = {
        {"host", vm["mqtt.host"].as<std::string>()},
	{"port", vm["mqtt.port"].as<int>()}
    };
    config["mqtt"] = mqtt;

    // add file object
    json::value file = {
        {"in", vm["file.in"].as<std::string>()},
	{"out", vm["file.out"].as<std::string>()}
    };
    config["file"] = file;

    // if the user wants the software version, show it and exit
    if (vm.count("version")) {
	std::string version = val<std::string>(config, "version");
        std::cout << version << std::endl;
        exit(EXIT_SUCCESS);
    }

    // Validate configuration
    validate(config);

    return config;
}

void Configurator::validate(json::object config){

    // both filenames must be specified or both empty
    json::object file = val<json::object>(config, "file");
    std::string file_in = val<std::string>(file, "in");
    std::string file_out = val<std::string>(file, "out");

    if((file_in.empty() &&!file_out.empty()) ||
        (!file_in.empty() &&file_out.empty())) 
    {
       std::cerr << "--file.in  = " << file_in << std::endl;
       std::cerr << "--file.out = " << file_out << std::endl;
       std::cerr << "need both specified for file mode" << std::endl;
       
       exit(EXIT_FAILURE);
    }

    // MQTT host must not be empty
    json::object mqtt = val<json::object>(config, "mqtt");
    std::string host = val<std::string>(mqtt, "host");

    if(host.empty()) {
       std::cerr << "--mqtt.host  = must not be empty" << std::endl;
       
       exit(EXIT_FAILURE);
    }
}

// return an options description of all command line inputs
po::options_description Configurator::describe_options(){

    po::options_description options("Configuration");
    options.add_options()
        ("help,h", "Display this help message")
        ("version,v", "Display the version number")
        ("config,c", 
            po::value<std::string>()->default_value("../config.json"),
            "Specify a config file"
        )
        ("mqtt.host",
            po::value<std::string>()->default_value("localhost"),
            "MQTT broker host"
        )
        ("mqtt.port",
            po::value<int>()->default_value(1883), 
            "MQTT broker port"
        )   
        ("file.in",
            po::value<std::string>()->default_value(""), 
            "Input filename"
        )   
        ("file.out",
            po::value<std::string>()->default_value(""), 
            "Output filename"
        )   
    ;

    return options;
}


// return the contents of the config file as a JSON object
json::object Configurator:: parse_config_file(std::string filename){

    // Read the config file as plaintext
    std::ifstream ifs(filename);
    if(!ifs) {
        BOOST_LOG_TRIVIAL(error) << "Could not read file: " << filename;
        exit(EXIT_FAILURE);
    }

    json::stream_parser p;
    json::error_code ec;

    // compose JSON from all lines in config file.  The file can
    // be in pretty or compact JSON format.
    for (std::string line; std::getline(ifs, line); ) {
        p.write_some(line, ec);
	if(ec) {
            BOOST_LOG_TRIVIAL(error) << "JSON error parsing: " << line;
            exit(EXIT_FAILURE);
        }
    }
    ifs.close();

    assert(p.done());
    return p.release().as_object();
}
