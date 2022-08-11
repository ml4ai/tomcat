#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "PublishedMessage.hpp"
#include "Message.hpp"

using namespace std;
namespace json = boost::json;

PublishedMessage::~PublishedMessage(){
}

PublishedMessage::PublishedMessage(string name, json::object config):
    Message(name, config) 
{
    source = json::value_to<string>(config.at("publication_source"));
}

/* create a json value without any testbed messages received */
json::value PublishedMessage::json_value(string timestamp){
    
    json::value data = data_json_value();

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
        data
    };

    return jv;
}

json::value PublishedMessage::data_json_value(){

    json::value jv = {
        {"data",  {
		  }}};

    return jv;
}


