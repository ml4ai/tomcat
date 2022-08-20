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


}

void ReferenceMessageHandler::process_message(const string topic, 
                                              const json::object &message) {

    BaseMessageHandler::process_message(topic, message);

    // our specifics here
}

vector<string> ReferenceMessageHandler::get_input_topics() {
    vector input_topics = BaseMessageHandler::get_input_topics();
    input_topics.push_back("agent/reference_agent_input_1");
    input_topics.push_back("agent/reference_agent_input_2");
    return input_topics;
}

vector<string> ReferenceMessageHandler::get_output_topics() {
    vector output_topics = BaseMessageHandler::get_output_topics();
    output_topics.push_back("agent/reference_agent_output_1");
    output_topics.push_back("agent/reference_agent_output_2");
    output_topics.push_back("agent/reference_agent_output_3");
    return output_topics;
}

    
