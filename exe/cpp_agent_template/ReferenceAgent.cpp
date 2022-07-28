#include "ReferenceAgent.hpp"

#include <nlohmann/json.hpp>
#include <iostream>

using namespace std;
using json = nlohmann::json;

void ReferenceAgent::process(mqtt::const_message_ptr msg) {
    json jv = json::parse(msg->to_string());

    // Uncomment the line below to print the message
    cout << jv << endl;
}

ReferenceAgent::ReferenceAgent(string address) : Agent(address){};
