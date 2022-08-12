#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "ReferenceAgentMessage.hpp"
#include "PublishedMessage.hpp"

using namespace std;
namespace json = boost::json;


ReferenceAgentMessage::ReferenceAgentMessage(json::object config):
    PublishedMessage("reference_agent_output", config) 
{
}

/* create a json value without any testbed messages received */
json::value ReferenceAgentMessage::data_json_value(){
    
    json::value jv = {"data", {
        {"key", "value"}}};

    return jv;
}
