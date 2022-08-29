#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <iostream>
#include <exception>
#include <mqtt/async_client.h>


#include "Agent.hpp"
#include "MqttAgent.hpp"

// This class :
//   Maintains the MQTT broker connection
//   Handles all traffic to and from the Message Bus

using namespace std;
namespace json = boost::json;
using namespace std::chrono;


MqttAgent::MqttAgent(const json::object &config) {

    cout << "Running in MQTT Mode" << endl;

    // set up MQTT params for broker connection
    json::object mqtt_config = json::value_to<json::object>(config.at("mqtt"));
    string host = json::value_to<string>(mqtt_config.at("host"));
    int port = json::value_to<int>(mqtt_config.at("port"));
    string address = "tcp://" + host + ":" + to_string(port);

    // Create an MQTT client smart pointer to be shared among threads.
    mqtt_client = make_shared<mqtt::async_client>(
        address,
       	"cpp_template_agent"
    );


    // Connect options for a non-persistent session and automatic
    // reconnects.
    auto connOpts = mqtt::connect_options_builder()
                        .clean_session(true)
                        .automatic_reconnect(seconds(2), seconds(30))
                        .finalize();

    mqtt_client->set_message_callback([&](mqtt::const_message_ptr m_ptr) {

        if(!running){
	    return;
        }

	string topic = m_ptr->get_topic();
	string text = m_ptr->get_payload_str();

	json::object obj = parse_json(text);
	obj["topic"] = topic;

	// enqueue message
	message_queue.push(obj);
    });

    try {
        auto rsp = mqtt_client->connect(connOpts)->get_connect_response();
    } catch (exception& e) {
        cerr << "Could not connect to MQTT Broker at " << address << endl;
	cerr << "Exception: " << e.what() << endl;
        exit(EXIT_FAILURE);
    }

    BOOST_LOG_TRIVIAL(info) << "Connected to the MQTT broker at " << address;

    Agent::configure(config);

    // subscribe to input topics
    for(string i : message_handler.get_input_topics()) {
        mqtt_client->subscribe(i, 2);
    }

    // send an early heartbeat to advise of configuration
    message_handler.publish_heartbeat_message();
}

// check the message queue every second
void MqttAgent::check_queue() {

    this_thread::sleep_for(seconds(1));

    while (running) {
        this_thread::sleep_for(seconds(1));
	if(running) {
            if((message_queue.size() > 0) && !busy) {
                process_next_message();
            }
        }
    }
}

void MqttAgent::process_next_message(){
    if(message_queue.empty()) {
        busy = false;
    } else {
        busy = true;
        const json::object &obj = message_queue.front();
        const json::object &copy = json::object(obj);
        message_queue.pop();
        string topic = val<string>(copy, "topic");
	int sz = message_queue.size();
	cout << "Processing " << topic << ", " << sz << " in queue" << endl;
        message_handler.process_message(copy);
    }
}


void MqttAgent::publish(json::object &message) {
    if(running) {
	string topic = val<string>(message, "topic");
	if(topic.empty()) {
            cerr << "MqttAgent::publish ERROR no topic in message" << endl;
            cerr << message << endl;
	    return;
	}
	message.erase("topic"); // do not publish topic with message on bus
        cout << "Publishing to " << topic << endl;
        mqtt_client->publish(topic, json::serialize(message));
    }
}

// begin the asynchronous message queue monitor and start 
void MqttAgent::start() {
    running = true;
    cout << app_name << " running." << endl;

    // start the asynchronous Message queue monitor
    queue_future = async(
        launch::async,
        &MqttAgent::check_queue,
        this
    );

    message_handler.start();
}

void MqttAgent::stop() {
    message_handler.stop();
    running = false;
    queue_future.wait();
    cout << app_name << " stopped." << endl;
    int sz = message_queue.size();
    if(sz > 0) {
        cout << "Messages left in processing queue: " << sz << endl;
    }
}
