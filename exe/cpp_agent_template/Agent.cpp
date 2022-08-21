#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "Agent.hpp"
#include "ReferenceMessageHandler.hpp"
#include <iostream>


using namespace std;
namespace json = boost::json;

Agent::Agent(const json::object &config) {
    message_handler = ReferenceMessageHandler(this, config);
}

void Agent::read(const string topic, const json::object &message) {
    message_handler.process_message(topic, message);
}

vector<string> Agent::get_input_topics() {
    return message_handler.get_topics("subscribes");
}

vector<string> Agent::get_output_topics() {
    return message_handler.get_topics("publishes");
}
