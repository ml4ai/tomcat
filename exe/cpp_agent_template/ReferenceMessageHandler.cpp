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

    // get subscription topics
    json::array subs = val<json::array>(config, "subscribes");
    for(size_t i = 0;  i < subs.size(); i++) {
	json::value element = subs.at(i);
	string topic = json::value_to<std::string>(element.at("topic"));
        input_topics.push_back(topic);
    }

    // get publication topics
    json::array pubs = val<json::array>(config, "publishes");
    for(size_t i = 0 ;  i < pubs.size() ; i++) {
	json::value element = pubs.at(i);
	string topic = json::value_to<std::string>(element.at("topic"));
        output_topics.push_back(topic);
    }
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
