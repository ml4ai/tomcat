#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "RollcallRequestProcessor.hpp"


using namespace std;
namespace json = boost::json;


string RollcallRequestProcessor::get_subscription_name(){
    return "rollcall_request";
}

string RollcallRequestProcessor::get_publication_name(){
    return "rollcall_response";
}

/* Process the rollcall request input from the message bus */
void RollcallRequestProcessor::process_input_message(
    json::object input_header,
    json::object input_msg,
    json::object input_data
) {

    // compose a rollcall response message

    string timestamp = utils.get_timestamp();
    int uptime_seconds = 1234;  // TODO get actual uptime

    json::value message = {
        {"header", 
            header(timestamp, input_header)},
	{"msg",
            msg(timestamp, input_msg)},
	{"data",  {
            {"rollcall_id", input_data.at("rollcall_id")},
	    {"status", "up"},
	    {"uptime", uptime_seconds},
	    {"version", version}}}
    };

    publish(message);
}
