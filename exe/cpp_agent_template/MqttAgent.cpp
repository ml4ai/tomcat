#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <exception>
#include <iostream>
#include <mqtt/async_client.h>

#include "Agent.hpp"
#include "MqttAgent.hpp"

// This class :
//   Maintains the MQTT broker connection
//   Handles all traffic to and from the Message Bus

namespace json = boost::json;

MqttAgent::MqttAgent(const json::object& config) {

    std::cout << "Running in MQTT Mode" << std::endl;

    // set up MQTT params for broker connection
    json::object mqtt_config = val<json::object>(config, "mqtt");
    std::string host = val<std::string>(mqtt_config, "host");
    int port = val<int>(mqtt_config, "port");
    std::string address = "tcp://" + host + ":" + std::to_string(port);
    std::string agent_name = val<std::string>(config, "agent_name");

    // Create an MQTT client smart pointer to be shared among threads.
    mqtt_client = make_shared<mqtt::async_client>(
        address, agent_name.empty() ? "cpp_template_agent" : agent_name);

    // Connect options for a non-persistent session and automatic
    // reconnects.
    auto connOpts = mqtt::connect_options_builder()
                        .clean_session(true)
                        .automatic_reconnect(std::chrono::seconds(2),
                                             std::chrono::seconds(30))
                        .finalize();

    mqtt_client->set_message_callback([&](mqtt::const_message_ptr m_ptr) {
        if (!running) {
            return;
        }

        json::object obj = parse_json(m_ptr->get_payload_str());
        obj["topic"] = m_ptr->get_topic();

        // enqueue message
        message_queue.push_back(obj);
    });

    try {
        auto rsp = mqtt_client->connect(connOpts)->get_connect_response();
    }
    catch (std::exception& e) {
        std::cerr << "Could not connect to MQTT Broker at ";
        std::cerr << address << std::endl;
        std::cerr << "Exception: " << e.what() << std::endl;
        exit(EXIT_FAILURE);
    }

    BOOST_LOG_TRIVIAL(info) << "Connected to the MQTT broker at " << address;

    Agent::configure(config);

    // subscribe to input topics
    for (std::string i : reference_processor.get_input_topics()) {
        mqtt_client->subscribe(i, 2);
    }

    // send an early heartbeat to advise of configuration
    reference_processor.publish_heartbeat_message();
}

// check the message queue every second
void MqttAgent::check_queue() {
    while (running) {
        std::this_thread::sleep_for(std::chrono::seconds(1));
        if (running) {
            if ((message_queue.size() > 0) && !busy) {
                process_next_message();
            }
        }
    }
}

void MqttAgent::process_next_message() {
    if (message_queue.empty()) {
        busy = false;
    }
    else {
        busy = true;
        const json::object& obj = message_queue.front();
        const json::object& copy = json::object(obj);
        message_queue.pop_front();
        std::string topic = val<std::string>(copy, "topic");
        int sz = message_queue.size();
        std::cout << "Processing " << topic << ", ";
        std::cout << sz << " in queue" << std::endl;
        reference_processor.process_message(copy);
    }
}

void MqttAgent::publish(json::object& message) {
    if (running) {
        std::string topic = val<std::string>(message, "topic");
        if (topic.empty()) {
            std::cerr << "MqttAgent::publish ERROR:";
            std::cerr << "No topic in message." << std::endl;
            std::cerr << message << std::endl;
            return;
        }
        message.erase("topic"); // do not publish topic with message on bus
        std::cout << "Publishing to " << topic << std::endl;
        mqtt_client->publish(topic, json::serialize(message));
    }
}

// begin the asynchronous message queue monitor and start
void MqttAgent::start() {
    running = true;
    std::cout << app_name << " running." << std::endl;
    // start the asynchronous Message queue monitor
    queue_future =
        std::async(std::launch::async, &MqttAgent::check_queue, this);
    reference_processor.start();
}

void MqttAgent::stop() {
    reference_processor.stop();
    running = false;
    queue_future.wait();
    std::cout << app_name << " stopped." << std::endl;
    int sz = message_queue.size();
    // advise if any input was unprocessed.
    if (sz > 0) {
        std::cout << "Unprocessed input messages left in queue: ";
        std::cout << sz << std::endl;
    }
}
