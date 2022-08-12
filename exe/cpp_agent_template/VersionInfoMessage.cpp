#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "VersionInfoMessage.hpp"
#include "PublishedMessage.hpp"

using namespace std;
namespace json = boost::json;


VersionInfoMessage::VersionInfoMessage(json::object config):
    PublishedMessage("version_info", config) 
{
}

/* create a json value without any testbed messages received */
json::value VersionInfoMessage::data_json_value(){
    
    json::value jv = {"data", {
        {"key", "value"}}};

    return jv;
}
