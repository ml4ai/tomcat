#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "RollcallResponseMessage.hpp"
#include "PublishedMessage.hpp"

using namespace std;
namespace json = boost::json;


RollcallResponseMessage::RollcallResponseMessage(json::object config):
    PublishedMessage("rollcall_response", config) 
{
}

/* create a json value without any testbed messages received */
json::value RollcallResponseMessage::data_json_value(){
    
    json::value jv = {"data", {
        {"key", "value"}}};

    return jv;
}
