#include "mqtt/async_client.h"
#include <chrono>
#include <csignal>
#include <fstream>
#include <functional>
#include <iostream>
#include <memory>
#include <string>
#include <thread>

#include "file.hpp"
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <boost/program_options.hpp>
#include <filesystem>

namespace po = boost::program_options;
namespace json = boost::json;
//namespace fs = boost::filesystem;

using namespace std;
using namespace std::chrono;

void publisher_func(mqtt::async_client_ptr cli) {
    while (true) {
        this_thread::sleep_for(seconds(1));
        cli->publish("status/agent", "ok")->wait();
    }
}

void subscriber_func(mqtt::async_client_ptr cli) {
    while (true) {
        auto msg = cli->consume_message();

        if (!msg) {
            continue;
        }

        json::value jv = json::parse(msg->to_string());
        cout << jv << endl;
        // cout << msg->get_topic() << ": " << msg->to_string() << endl;
    }
}

int main(int argc, char* argv[]) {

    po::options_description generic("Generic options");

    string config_path;
    generic.add_options()("help,h", "Display this help message")(
        "version,v", "Display the version number")(
        "config,c", po::value<string>(&config_path)->default_value("config.ini"));

    po::options_description config("Configuration");

    config.add_options()("mqtt.host",
                         po::value<string>()->default_value("localhost"),
                         "MQTT broker host")(
        "mqtt.port", po::value<int>()->default_value(1883), "MQTT broker port");

    po::options_description cmdline_options;
    cmdline_options.add(generic).add(config);

    po::variables_map vm;
    po::store(po::parse_command_line(argc, argv, cmdline_options), vm);
    po::notify(vm);

    if (vm.count("help")) {
        std::cout << cmdline_options;
        return 1;
    }

    if (filesystem::exists(config_path)) {
        po::store(po::parse_config_file(config_path.c_str(), config), vm);
    }
    else {
        BOOST_LOG_TRIVIAL(error) << "Specified config file '" << config_path << "' does not exist!";
        return EXIT_FAILURE;
    }

    po::notify(vm);


    string address = "tcp://" + vm["mqtt.host"].as<string>() + ":" +
                     to_string(vm["mqtt.port"].as<int>());

    // Create an MQTT client using a smart pointer to be shared among threads.
    auto client = make_shared<mqtt::async_client>(address, "agent");

    // Connect options for a persistent session and automatic reconnects.
    auto connOpts = mqtt::connect_options_builder()
                        .clean_session(false)
                        .automatic_reconnect(seconds(2), seconds(30))
                        .finalize();

    auto topics = mqtt::string_collection::create({"test"});
    const vector<int> QOS{2};

    try {
        // Start consuming _before_ connecting, because we could get a flood
        // of stored messages as soon as the connection completes since
        // we're using a persistent (non-clean) session with the broker.
        client->start_consuming();

        BOOST_LOG_TRIVIAL(info)
            << "Connecting to the MQTT broker at " << address << "...";
        auto rsp = client->connect(connOpts)->get_connect_response();
        BOOST_LOG_TRIVIAL(info) << "Connected.";

        // Subscribe if this is a new session with the server
        if (!rsp.is_session_present()) {
            client->subscribe(topics, QOS);
        }

        // Start the publisher thread

        thread publisher(publisher_func, client);

        // Consume messages in this thread
        thread subscriber(subscriber_func, client);

        publisher.join();
        subscriber.join();

        // Disconnect

        BOOST_LOG_TRIVIAL(info) << "Disconnecting...";
        client->disconnect();
        BOOST_LOG_TRIVIAL(info) << "Disconnected";
    }
    catch (const mqtt::exception& exc) {
        BOOST_LOG_TRIVIAL(error) << exc.what();
        return 1;
    }

    return EXIT_SUCCESS;
}
