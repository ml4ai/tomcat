#include "ReferenceAgent.hpp"

#include <boost/json.hpp>
#include <iostream>

using namespace std;
namespace json = boost::json;

/** this is the method input messages are processed by the agent */
void ReferenceAgent::process(mqtt::const_message_ptr msg) {

    string topic = msg->get_topic();

    cout << "Message received on topic " << topic << endl;
    string msgstr = msg->to_string();

    /*
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
    */

}

ReferenceAgent::ReferenceAgent(json::object config): BaseAgent(config)
{
    cout << "Reference Agent" << endl;
//    cout << " Input topic: " << input_topic << endl;
//    cout << " Output topic: " << output_topic << endl;
};
