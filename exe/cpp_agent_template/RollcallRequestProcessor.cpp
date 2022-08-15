#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "RollcallRequestProcessor.hpp"

using namespace std;
namespace json = boost::json;

void RollcallRequestProcessor::configure(json::object config) {

    // input
    rollcall_request_config = utils.get_object("rollcall_request", config);
    rollcall_request_topic = utils.get_string("topic", rollcall_request_config);
    cout << "RollcallRequestProcessor subscribing to " << rollcall_request_topic << endl;
    mqtt_client->subscribe(rollcall_request_topic, 2);

    // output
    rollcall_response_config = utils.get_object("rollcall_response", config);
    rollcall_response_topic = utils.get_string("topic", rollcall_response_config);
    rollcall_response_message_type = utils.get_string("message_type", rollcall_response_config);
    rollcall_response_sub_type = utils.get_string("sub_type", rollcall_response_config);

    cout << "RollcallRequestProcessor publishing on " << rollcall_response_topic << endl;
}

/* Process the rollcall request input from the message bus */
void RollcallRequestProcessor::process_input_message(
    string topic,
    json::object input_message
) {

    if(rollcall_request_topic.compare(topic) != 0) {
        return;
    }

    cout << "RollcallRequestProcessor::process_input_message" << endl;

    json::object input_header = utils.get_object("header", input_message);
    json::object input_msg = utils.get_object("msg", input_message);
    json::object input_data = utils.get_object("data", input_message);


    if(utils.value_matches(rollcall_request_config, input_header, "message_type") &&
        utils.value_matches(rollcall_request_config, input_msg, "sub_type")) {
    
    string timestamp = utils.get_timestamp();
    int uptime_seconds = 1234;  // TODO get actual uptime

    json::value message = {
        {"header", 
            header(timestamp, input_header)},
	{"msg",
            msg(timestamp, input_msg)},
	{"data",  {
            {"rollcall_id", utils.get_string("rollcall_id", input_data)},
	    {"status", "up"},
	    {"uptime", uptime_seconds},
	    {"version", version}}}
    };

    publish(rollcall_response_topic, message);
}
}
