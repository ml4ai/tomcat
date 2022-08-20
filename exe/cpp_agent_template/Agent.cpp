#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "Agent.hpp"

// This class :

using namespace std;
namespace json = boost::json;

Agent::Agent(json::object config) {
    message_handler.configure(config, this);
}

void Agent::process_message(string topic, json::object message) {
    message_handler.process_message(topic, message);
}
