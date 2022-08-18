#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "Agent.hpp"

// This class :
//   Maintains the MQTT broker connection
//   Maintains message handlers 
//   Subscribes (reads) from and publishes (writes) to the Message Bus
//

using namespace std;
namespace json = boost::json;


void Agent::configure(json::object config) {
    message_handler.configure(config, this);
}

void Agent::process_message(string topic, json::object message) {
    message_handler.process_message(topic, message);
}

vector<string> Agent::get_input_topics() {
    return message_handler.get_subscriptions();
}

vector<string> Agent::get_output_topics() {
    return message_handler.get_publications();
}

void Agent::stop() {
    message_handler.stop();
}

void Agent::start() {
    message_handler.stop();
}
