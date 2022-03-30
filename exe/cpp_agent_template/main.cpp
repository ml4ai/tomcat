// async_subscribe.cpp
//
// This is a Paho MQTT C++ client, sample application.
//
// This application is an MQTT publisher/subscriber using the C++
// asynchronous client interface, demonstrating how you can share a client
// between multiple threads.
//
// The app will count the number of "data" messages arriving at the broker
// and then emit "events" with updated counts. A data message is any on a
// "data/#" topic, and counts are emitted on the "events/count" topic. It
// emits an event count around once every ten data messages.
//
// Note that this is a fairly contrived example, and it could be done much
// more easily in a single thread. It is meant to demonstrate how you can
// share a client amonst threads if and when that's a proper thing to do.
//
// At this time, there is a single callback or consumer queue for all
// incoming messages, so you would typically only have one thead receiving
// messages, although it _could_ send messages to multiple threads for
// processing, perhaps based on the topics. It could be common, however, to
// want to have multiple threads for publishing.
//
// The sample demonstrates:
//  - Creating a client and accessing it from a shared_ptr<>
//  - Using one thread to receive incoming messages from the broker and
//    another thread to publish messages to it.
//  - Connecting to an MQTT server/broker.
//  - Subscribing to a topic
//  - Using the asynchronous consumer
//  - Publishing messages.
//

#include "mqtt/async_client.h"
#include <cctype>
#include <chrono>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <memory>
#include <string>
#include <thread>

#include <boost/log/trivial.hpp>
#include <boost/program_options.hpp>
namespace po = boost::program_options;

using namespace std;
using namespace std::chrono;

const string DFLT_SERVER_ADDRESS("tcp://localhost:1883");
const string CLIENT_ID("multithr_pub_sub_cpp");

const auto SAMPLE_PERIOD = milliseconds(5);
/////////////////////////////////////////////////////////////////////////////

// The MQTT publisher function will run in its own thread.
// It runs until the receiver thread closes the counter object.
void publisher_func(mqtt::async_client_ptr cli) {
    while (true) {
        this_thread::sleep_for(SAMPLE_PERIOD);
        cli->publish("test", "test_message")->wait();
    }
}

void subscriber_func(mqtt::async_client_ptr cli) {
    while (true) {
        auto msg = cli->consume_message();

        if (!msg)
            continue;

        if (msg->get_topic() == "command" && msg->to_string() == "exit") {
            cout << "Exit command received" << endl;
            break;
        }

        cout << msg->get_topic() << ": " << msg->to_string() << endl;
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

    string address = (argc > 1) ? string(argv[1]) : DFLT_SERVER_ADDRESS;

    // Create an MQTT client using a smart pointer to be shared among threads.
    auto cli = make_shared<mqtt::async_client>(address, CLIENT_ID);

    // Connect options for a persistent session and automatic reconnects.
    auto connOpts = mqtt::connect_options_builder()
                        .clean_session(false)
                        .automatic_reconnect(seconds(2), seconds(30))
                        .finalize();

    auto TOPICS = mqtt::string_collection::create({"data/#", "command"});
    const vector<int> QOS{0, 1};

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
        if (!rsp.is_session_present())
            cli->subscribe(TOPICS, QOS);

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
