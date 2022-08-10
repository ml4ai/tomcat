#include "Message.hpp"
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>

using namespace std;
namespace json = boost::json;

Message::Message(string name, json::object config) {
    this->name = name;

    version = json::value_to<string>(config.at("version"));

    json::object info = json::value_to<json::object>(config.at(name));

    topic = json::value_to<string>(info.at("topic"));
    message_type = json::value_to<string>(info.at("message_type"));
    sub_type = json::value_to<string>(info.at("sub_type"));
}
