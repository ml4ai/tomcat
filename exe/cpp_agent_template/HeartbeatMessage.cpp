#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "HeartbeatMessage.hpp"
#include "PublishedMessage.hpp"

using namespace std;
namespace json = boost::json;


HeartbeatMessage::HeartbeatMessage(json::object config):
    PublishedMessage("heartbeat", config) 
{
}

/* create a json value without any testbed messages received */
json::value HeartbeatMessage::data_json_value(){
    
    json::value jv = {"data", {
        {"state", state}}};

    return jv;
}
