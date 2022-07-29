#include "ReferenceAgent.hpp"

#include <boost/json.hpp>
#include <iostream>

using namespace std;
namespace json = boost::json;

void ReferenceAgent::process(mqtt::const_message_ptr msg) {

    string topic = msg->get_topic();

    cout << "Message received on topic " << topic << endl;
    string msgstr = msg->to_string();

//    try {
	json::object jv = json::parse(msgstr).as_object();
        cout << jv << endl;
//    }
//    catch (nlohmann::detail::parse_error) {
//        cout << "Could not parse " << msgstr << endl;

//    }

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
