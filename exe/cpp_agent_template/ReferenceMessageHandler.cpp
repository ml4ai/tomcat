#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "ReferenceMessageHandler.hpp"
#include "BaseMessageHandler.hpp"
#include "Agent.hpp"


using namespace std;
namespace json = boost::json;

ReferenceMessageHandler::ReferenceMessageHandler(
    Agent *agent,
    const json::object &config) :  BaseMessageHandler(agent, config)
{


}

void ReferenceMessageHandler::process_message(const string topic, 
                                              const json::object &message) {

    BaseMessageHandler::process_message(topic, message);

    string timestamp = get_timestamp();
    json::object out_header = get_output_header(message, timestamp);
    json::object out_msg = get_output_msg(message, timestamp);
    


    // our specifics here
}
