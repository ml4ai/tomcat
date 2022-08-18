#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <boost/json/array.hpp>
#include <mqtt/async_client.h>
#include <boost/date_time/posix_time/posix_time.hpp>

#include "Agent.hpp"

#include "MessageHandler.hpp"

using namespace std;
namespace json = boost::json;

/** Get current UTC timestamp in ISO-8601 format. */
string MessageHandler::get_timestamp() {
    return boost::posix_time::to_iso_extended_string(
        boost::posix_time::microsec_clock::universal_time()
    ) + "Z";
}

// Set parameters using the configuration
void MessageHandler::configure(json::object config, Agent *agent) {

    this->agent = agent;

    this->config = config;

    // find subscriptions
    json::array subs = val<json::array>(config, "subscribes");

    cout << "subs" << subs.size() << endl;



}

bool MessageHandler::is_subscribed(string topic){
    for(string t : subscriptions) {
        if(t.compare(topic) == 0) {
	    return true;
	}
    }
    return false;
}
	

void MessageHandler::process_message(
    string topic,
    json::object input_message
) {

    if(!running) {
        return;
    }

    /* topic must match our configuration */
    if(!is_subscribed(topic)) {
        return;
    }

    process_header(input_message, json::object(), get_timestamp());
}

void MessageHandler::process_header(
    json::object input_message,
    json::object output_message,
    string timestamp
){
    json::object input_header = 
        val<json::object>(input_message, "header");

    if(!valid_input_header(input_header)){
        return;
    }

    // if the input header has no testbed version number, default to 1.0
    string version_maybe = val<string>(input_header, "version");
    string testbed_version = version_maybe.empty()? "1.0": version_maybe;

    json::object header;
    header["timestamp"] = timestamp;
    header["message_type"] = get_output_message_type(); // user specific
    header["version"] = testbed_version;

    output_message["header"] = header;

    process_msg(input_message, output_message, timestamp);
}

// true if header.message_type matches our configuration
bool MessageHandler::valid_input_header(json::object input_header) {

    return true;
    /*
    string message_type = val<string>(input_header, "message_type");
    return (input_message_type.compare(message_type) == 0);
    */
}

// true if msg.sub_type matches our configuration
bool MessageHandler::valid_input_msg(json::object input_msg) {
    
    return true;

    /*
    string sub_type = val<string>(input_msg, "sub_type");
    return (input_sub_type.compare(sub_type) == 0);
    */
}


void MessageHandler::process_msg(
    json::object input_message,
    json::object output_message,
    string timestamp
){
    // msg.sub_type must match our configuration
    json::object input_msg = 
        val<json::object>(input_message, "msg");

    if(!valid_input_msg(input_msg)) {
        return;
    }

    json::object msg;
    msg["source"] = "template_source";
    msg["version"] = "template_version";
    msg["timestamp"] = timestamp;

    // these may be empty
    msg = update_nonempty_string(input_msg, msg, "experiment_id");
    msg = update_nonempty_string(input_msg, msg, "trial_id");
    msg = update_nonempty_string(input_msg, msg, "replay_root_id");
    msg = update_nonempty_string(input_msg, msg, "replay_id");

    output_message["msg"] = msg;

    process_data(input_message, output_message);
}

void MessageHandler::process_data(
    json::object input_message,
    json::object output_message
){
    json::object input_data = 
        val<json::object>(input_message, "data");

    /** Data processing happens here */

    // default just to show something is the versioninfo data
    output_message["data"] = config;

    // output topic from config


    agent->write("template_agent_topic", output_message);
}

// copy string, delete key from dst if string is empty
json::object MessageHandler::update_nonempty_string(
    json::object src,
    json::object dst,
    string key
) {
    dst[key] = val<string>(src, key);
    if(val<string>(dst, key).empty()) {
        dst.erase(key);
    }
    return dst;
}
