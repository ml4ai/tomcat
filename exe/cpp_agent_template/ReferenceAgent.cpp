#include "ReferenceAgent.hpp"

#include <boost/json.hpp>
#include <iostream>

using namespace std;
namespace json = boost::json;

void ReferenceAgent::process(mqtt::const_message_ptr msg) {

    string topic = msg->get_topic();

    cout << "Message received on topic " << topic << endl;
    string msgstr = msg->to_string();

    try {
	json::object jv = json::parse(msgstr).as_object();
        cout << msgstr << endl;
        string processed_input = "Processed " + msgstr;

        mqtt_client->publish(output_topic, processed_input);
    }
    catch(exception const& e)
    {
        std::cerr <<
            "Could not process message.  Caught exception: "
            << e.what() << std::endl;
    }

}

ReferenceAgent::ReferenceAgent(
    string host,
    int port,
    string input_topic,
    string output_topic
) : AgentBase(host, port, input_topic, output_topic),
        input_topic(input_topic), 
        output_topic(output_topic) 
{
    cout << "Reference Agent" << endl;
    cout << " Input topic: " << input_topic << endl;
    cout << " Output topic: " << output_topic << endl;
};
