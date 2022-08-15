#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "ReferenceAgentProcessor.hpp"


using namespace std;
namespace json = boost::json;


void ReferenceAgentProcessor::configure(json::object config) {
    // input
    input_config = utils.get_object("reference_agent_input", config);
    input_topic = utils.get_string("topic", input_config);
    mqtt_client->subscribe(input_topic, 2);
    cout << "ReferenceAgentProcessor subscribing to " << input_topic << endl;

    // output
    output_config = utils.get_object("reference_agent_output", config);
    output_topic = utils.get_string("topic", output_config);
    output_message_type = utils.get_string("message_type", output_config);
    output_sub_type = utils.get_string("sub_type", output_config);
    cout << "ReferenceAgentProcessor publishing on " << output_topic << endl;
}


/* Process the input */
void ReferenceAgentProcessor::process_input_message(
    string topic,
    json::object input_message
) {
    if(input_topic.compare(topic) != 0) {
        return;
    }

    cout << "ReferenceAgentProcessor::process_input_message" << endl;

    json::object input_header = utils.get_object("header", input_message);
    json::object input_msg = utils.get_object("msg", input_message);
    json::object input_data = utils.get_object("data", input_message);

    if(!utils.value_matches(input_config, input_header, "message_type") ||
        !utils.value_matches(input_config, input_msg, "sub_type")) {
        return;
    }


    string timestamp = utils.get_timestamp();

    json::value message = {
        {"header", 
            header(timestamp, output_message_type, input_header)},
	{"msg",
            msg(timestamp, output_sub_type, input_msg)},
	{"data",  {
            {"reference_agent_data_key", "reference_agent_data_value"},
	    {"version", version}}}
    };

    publish(output_topic, message);
}
