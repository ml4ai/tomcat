#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "RollcallRequestProcessor.hpp"

using namespace std;
namespace json = boost::json;

void RollcallRequestProcessor::configure(json::object config) {

    // input
    input_config = utils.get_object("rollcall_request", config);
    input_topic = utils.get_string("topic", input_config);
    cout << "RollcallRequestProcessor subscribing to " << input_topic << endl;
    mqtt_client->subscribe(input_topic, 2);

    // output
    output_config = utils.get_object("rollcall_response", config);
    output_topic = utils.get_string("topic", output_config);
    cout << "RollcallRequestProcessor publishing on " << output_topic << endl;
}

/* Process the rollcall request input from the message bus */
void RollcallRequestProcessor::process_input_message(
    string topic,
    json::object input_message
) {

    if(input_topic.compare(topic) != 0) {
        return;
    }

    cout << "RollcallRequestProcessor::process_input_message" << endl;

    json::object input_header = utils.get_object("header", input_message);
    json::object input_msg = utils.get_object("msg", input_message);
    json::object input_data = utils.get_object("data", input_message);

    if(!utils.value_matches(input_config, input_header, "message_type") ||
        !utils.value_matches(input_config, input_msg, "sub_type")) {
        return;
    }
    
    string timestamp = utils.get_timestamp();
    int uptime_seconds = 1234;  // TODO get actual uptime

    json::value message = {
        {"header", 
            header(timestamp, output_config, input_header)},
	{"msg",
            msg(timestamp, output_config, input_msg)},
	{"data",  {
            {"rollcall_id", utils.get_string("rollcall_id", input_data)},
	    {"status", "up"},
	    {"uptime", uptime_seconds},
	    {"version", version}}}
    };

    publish(output_topic, message);
}
