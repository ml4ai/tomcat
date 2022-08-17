#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "MqttAgent.hpp"
#include <set>

// This class :
//   Maintains the MQTT broker connection
//   Maintains message handlers 
//   Subscribes (reads) from and publishes (writes) to the Message Bus
//

using namespace std;
namespace json = boost::json;
using namespace std::chrono;


MqttAgent::MqttAgent(json::object config) {

    // set up MQTT params for broker connection
    json::object mqtt_config = json::value_to<json::object>(config.at("mqtt"));
    string host = json::value_to<string>(mqtt_config.at("host"));
    int port = json::value_to<int>(mqtt_config.at("port"));
    string address = "tcp://" + host + ": " + to_string(port);

    // Create an MQTT client smart pointer to be shared among threads.
    this->mqtt_client = make_shared<mqtt::async_client>(
        address, 
	"cpp_template_agent"
    );

    // Connect options for a non-persistent session and automatic
    // reconnects.
    auto connOpts = mqtt::connect_options_builder()
                        .clean_session(true)
                        .automatic_reconnect(seconds(2), seconds(30))
                        .finalize();

    mqtt_client->set_message_callback([&](mqtt::const_message_ptr message_ptr) {
        for(int i = 0; i < N_MESSAGE_HANDLERS; i ++) {
            message_handlers[i]->process_message(
                message_ptr->get_topic(),
                message_ptr
            );
	    
        }
    });

    auto rsp = this->mqtt_client->connect(connOpts)->get_connect_response();
    BOOST_LOG_TRIVIAL(info) << "Connected to the MQTT broker at " << address;

    // configure message_handlers
    for(int i = 0; i < N_MESSAGE_HANDLERS; i ++) {
        message_handlers[i]->configure(config, mqtt_client);
    }

    // Subscribe to and report subscription topics
    std::set<string> input;
    for(int i = 0; i < N_MESSAGE_HANDLERS; i ++) {
	input.insert(message_handlers[i]->get_input_topic());
    }
    for(std::set<string>::iterator i=input.begin(); i!=input.end(); ++i) {
	string input_topic = *i;
        mqtt_client->subscribe(input_topic, 2);
        cout << "Subscribed to: " << input_topic << endl;
    }

    // report publication topics
    std::set<string> output;
    for(int i = 0; i < N_MESSAGE_HANDLERS; i ++) {
	output.insert(message_handlers[i]->get_output_topic());
    }
    for(std::set<string>::iterator i=output.begin(); i!=output.end(); ++i) {
	string output_topic = *i;
        mqtt_client->subscribe(output_topic, 2);
        cout << "Publishing on: " << output_topic << endl;
    }
}

void MqttAgent::stop() {
    for(int i = 0; i < N_MESSAGE_HANDLERS; i ++) {
        message_handlers[i]-> stop();
    }
}

void MqttAgent::start() {
    for(int i = 0; i < N_MESSAGE_HANDLERS; i ++) {
        message_handlers[i]-> start();
    }
}
