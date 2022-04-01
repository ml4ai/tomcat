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

namespace po = boost::program_options;
namespace json = boost::json;

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

json::value parse_json_file(char const* filename) {
    file f(filename, "r");
    json::stream_parser p;
    json::error_code ec;
    do {
        char buf[4096];
        auto const nread = f.read(buf, sizeof(buf));
        p.write(buf, nread, ec);
    } while (!f.eof());
    if (ec)
        return nullptr;
    p.finish(ec);
    if (ec)
        return nullptr;
    return p.release();
}

int main(int argc, char* argv[]) {

    json::value config = parse_json_file("config.json");
    cout << config << endl;
    json::object obj = config.as_object();

    json::string address = "tcp://";
    address+= obj.at("mqtt_host").as_string();// + ":" + obj.at("mqtt_port").as_uint64();

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
