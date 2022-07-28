#include "ReferenceAgent.hpp"

#include <nlohmann/json.hpp>
#include <iostream>

using namespace std;
using json = nlohmann::json;

void ReferenceAgent::process(mqtt::const_message_ptr msg) {

    string topic = msg->get_topic();

    cout << "Message received on topic " << topic << endl;
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

ReferenceAgent::ReferenceAgent(
    string address,
    string input_topic,
    string output_topic
) : Agent(address, input_topic, output_topic){
    cout << "Reference Agent" << endl;
    cout << " Input topic: " << input_topic << endl;
    cout << " Output topic: " << output_topic << endl;
};
