#include "ReferenceAgent.hpp"

#include <nlohmann/json.hpp>
#include <iostream>

using namespace std;
using json = nlohmann::json;

void ReferenceAgent::process(mqtt::const_message_ptr msg) {
    string msgstr = msg->to_string();

    try {
        json jv = json::parse(msgstr);
        cout << jv << endl;
    }
    catch (nlohmann::detail::parse_error) {
        cout << "Could not parse " << msgstr << endl;

    }

    // Uncomment the line below to print the message
}

ReferenceAgent::ReferenceAgent(string address) : Agent(address){};
