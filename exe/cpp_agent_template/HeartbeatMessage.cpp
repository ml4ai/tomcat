#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "HeartbeatMessage.hpp"
#include "Message.hpp"

using namespace std;
namespace json = boost::json;


HeartbeatMessage::HeartbeatMessage(json::object config):
    Message("heartbeat", config) 
{
    source = json::value_to<string>(config.at("publication_source"));
}

/* create a json value without any testbed messages received */
json::value HeartbeatMessage::to_json_value(string timestamp){
    
    json::value jv = {
        {"header",  {
            {"message_type", message_type},
            {"timestamp", timestamp},
            {"version", "0.1"}}},
        {"msg",  {
            {"sub_type", sub_type},
            {"timestamp", timestamp},
            {"source", source},
            {"version", version}}},
        {"data", {
            {"state", state}}}
    };

    return jv;
}

