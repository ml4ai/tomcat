#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "Agent.hpp"
#include "MqttAgent.hpp"
#include <iostream>
#include <exception>


// This class :
//   Maintains the MQTT broker connection
//

using namespace std;
namespace json = boost::json;
using namespace std::chrono;


MqttAgent::MqttAgent(const json::object &config) : Agent(config) {

    cout << "Initializing C++ Template MQTT Agent..." << endl;

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

	process_message(topic, parse_json(text));
    });

    try {
        auto rsp = mqtt_client->connect(connOpts)->get_connect_response();
    } catch (exception& e) {
        cerr << "Could not connect to MQTT Broker at " << address << endl;
	cerr << "Exception: " << e.what() << endl;
        exit(EXIT_FAILURE);
    }

    BOOST_LOG_TRIVIAL(info) << "Connected to the MQTT broker at " << address;

    // subscribe to input topics
    for(string i : get_input_topics()) {
        mqtt_client->subscribe(i, 2);
    }
}

void MqttAgent::publish(const string topic, json::object &message) {
    publish(topic, json::serialize(message));
}

void MqttAgent::publish(const string topic, const string text) {
    if(running) {
        cout << "MqttAgent publishing on " << topic << endl;
        mqtt_client->publish(topic, text);
    }
}

void MqttAgent::start() {
    running = true;
    cout << "C++ Template Agent version " << version << " running." << endl;
    Agent::start();
}

void MqttAgent::stop() {
    Agent::stop();
    running = false;
    cout << "C++ Template Agent version " << version << " stopped." << endl;
}
