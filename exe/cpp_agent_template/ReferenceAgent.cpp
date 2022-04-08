#include "ReferenceAgent.hpp"

#include "boost/json.hpp"
#include <iostream>

using namespace std;
namespace json = boost::json;

void ReferenceAgent::process(mqtt::const_message_ptr msg) {
    json::object jv = json::parse(msg->to_string()).as_object();

    // Uncomment the line below to print the message
    cout << jv << endl;
}

ReferenceAgent::ReferenceAgent(string address) : Agent(address){};
