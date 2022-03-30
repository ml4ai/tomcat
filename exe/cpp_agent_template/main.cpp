#include "mqtt/async_client.h"
#include <chrono>
#include <iostream>
#include <memory>
#include <string>
#include <thread>
#include <csignal>
#include <functional>

#include <boost/log/trivial.hpp>
#include <boost/program_options.hpp>
#include <boost/json.hpp>
namespace po = boost::program_options;
namespace json = boost::json;

using namespace std;
using namespace std::chrono;


void publisher_func(mqtt::async_client_ptr cli) {
    while (true) {
        this_thread::sleep_for(seconds(1));
        cli->publish("heartbeats", "heartbeats")->wait();
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
        //cout << msg->get_topic() << ": " << msg->to_string() << endl;

    }
}

/////////////////////////////////////////////////////////////////////////////

int main(int argc, char* argv[]) {

    string mqtt_host;
    int mqtt_port;
    po::options_description desc{"Options"};
    desc.add_options()("help,h", "Help screen")(
        "mqtt_host,t",
        po::value<string>(&mqtt_host)->default_value("localhost"),
        "The host address of the MQTT broker")(
        "mqtt_port,p",
        po::value<int>(&mqtt_port)->default_value(1883),
        "The port of the MQTT broker");

    po::variables_map vm;
    try {
        po::store(po::parse_command_line(argc, argv, desc), vm);
    }
    catch (const exception& e) {
        BOOST_LOG_TRIVIAL(error) << "Error parsing arguments!" << e.what();
        return -1;
    }
    po::notify(vm);
    if (vm.count("help")) {
        cout << desc << "\n";
        return 1;
    }

    string address = "tcp://" + mqtt_host + ":" + to_string(mqtt_port);

    // Create an MQTT client using a smart pointer to be shared among threads.
    auto cli = make_shared<mqtt::async_client>(address, "agent");

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
        cli->start_consuming();

        cout << "Connecting to the MQTT server at " << address << "..."
             << flush;
        auto rsp = cli->connect(connOpts)->get_connect_response();
        cout << "OK\n" << endl;

        // Subscribe if this is a new session with the server
        if (!rsp.is_session_present()) {
            cli->subscribe(topics, QOS);
        }

        // Start the publisher thread

        thread publisher(publisher_func, cli);

        // Consume messages in this thread
        thread subscriber(subscriber_func, cli);

        publisher.join();
        subscriber.join();

        // Disconnect

        cout << "OK\nDisconnecting..." << flush;
        // cli->disconnect();
        cout << "OK" << endl;
    }
    catch (const mqtt::exception& exc) {
        cerr << exc.what() << endl;
        return 1;
    }

    return EXIT_SUCCESS;
}
