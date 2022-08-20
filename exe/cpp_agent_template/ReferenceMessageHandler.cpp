#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "ReferenceMessageHandler.hpp"
#include "BaseMessageHandler.hpp"
#include "Agent.hpp"


using namespace std;
namespace json = boost::json;

ReferenceMessageHandler::ReferenceMessageHandler(
    Agent *agent,
    const json::object &config): BaseMessageHandler(agent, config)
{

    // get config subscribes and publishes arrays

    input_topics.push_back("agent/reference_agent_input_1");
    input_topics.push_back("agent/reference_agent_input_2");

    output_topics.push_back("agent/reference_agent_output_1");
    output_topics.push_back("agent/reference_agent_output_2");
}

void ReferenceMessageHandler::process_message(const string topic, 
                                              const json::object &message) {

    
    BaseMessageHandler::process_message(topic, message);

    // our specifics here
}

vector<string> ReferenceMessageHandler::get_input_topics() {
    vector<string> all_input_topics;
    for(string i: BaseMessageHandler::get_input_topics()) {
        all_input_topics.push_back(i);
    }
    for(string i: input_topics) {
        all_input_topics.push_back(i);
    }
    return all_input_topics;
}

vector<string> ReferenceMessageHandler::get_output_topics() {
    vector<string> all_output_topics;
    for(string i: BaseMessageHandler::get_output_topics()) {
        all_output_topics.push_back(i);
    }
    for(string i: output_topics) {
        all_output_topics.push_back(i);
    }
    return all_output_topics;
}
