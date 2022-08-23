#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "Agent.hpp"
#include "ReferenceMessageHandler.hpp"
#include <iostream>


using namespace std;
namespace json = boost::json;

Agent::Agent(const json::object &config) {

    message_handler.configure(config);

    version = json::value_to<string>(config.at("version"));

    // advise of subscribed topics
    cout << "Subscription topics:" << endl;
    for(string i : message_handler.get_input_topics()) {
        cout << "    " << i << endl;
    }

    // advise of published topics
    cout << "Publication topics:" << endl;
    for(string i : message_handler.get_output_topics()) {
        cout << "    " << i << endl;
    }

}

void Agent::start(){
    message_handler.start_heartbeats();
}


void Agent::stop(){
    message_handler.stop_heartbeats();
}

void Agent::process_message(const string topic, 
                            const json::object &message) {
    message_handler.process_message(topic, message);
}

vector<string> Agent::get_input_topics(){
    return message_handler.get_input_topics();
}

vector<string> Agent::get_output_topics(){
    return message_handler.get_output_topics();
}
