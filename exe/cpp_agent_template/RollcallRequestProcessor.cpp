#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "RollcallRequestProcessor.hpp"


using namespace std;
namespace json = boost::json;


void RollcallRequestProcessor::process_subscribed_message(
    json::object sub_header,
    json::object sub_msg,
    json::object sub_data
) {
    cout << "RollcallRequestProcessor::process_subscribed_message" << endl;

    // compose a rollcall response message
    string timestamp = "foo";
    int uptime_seconds = 1234;

    json::value message = {
        {"header", header(timestamp, sub_header)},
	{"msg", msg(timestamp, sub_msg)},
	{"data", 
            {"rollcall_id", sub_data.at("rollcall_id")},
	    {"status", "up"},
	    {"uptime", uptime_seconds}
	}
    };

    publish(message);

    /*

    json::value header = header(timestamp, pub_config);
    json::value msg = msg(timestamp, pub_config, sub_msg);

    header["message_type"] = pub_config.message_type;
    header["timestamp"] = timestamp;
    header["version"] = version;

    json::object msg;
    msg["experiment_id"] = sub_msg.at("experiment_id");
    msg["timestamp"] = timestamp;
    msg["source"] = source;
    msg["sub_type"] = pub_config.sub_type;
    msg["version"] = version;

    // msg fields that may or may not be present
    if(sub_msg.contains("trial_id")) {
        msg["trial_id"] = sub_msg.at("trial_id");
    }
    if(sub_msg.contains("replay_root_id")) {
        msg["replay_root_id"] = sub_msg.at("replay_root_id");
    }
    if(sub_msg.contains("replay_id")) {
        msg["replay_id"] = sub_msg.at("replay_id");
    }

    json::object data;
    data["rollcall_id"] = sub_data.at("rollcall_id");
    data["status"] = "up";
    data["uptime"] = uptime_seconds;
    */


}
