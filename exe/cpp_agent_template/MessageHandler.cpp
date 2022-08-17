#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <boost/json/array.hpp>
#include <mqtt/async_client.h>
#include <boost/date_time/posix_time/posix_time.hpp>


#include "MessageHandler.hpp"

using namespace std;
namespace json = boost::json;

/** Get current UTC timestamp in ISO-8601 format. */
string MessageHandler::get_timestamp() {
    return boost::posix_time::to_iso_extended_string(
               boost::posix_time::microsec_clock::universal_time()) +
           "Z";
}

// Set parameters using the config file input. 
void MessageHandler::configure(
    json::object config,
    std::shared_ptr<mqtt::async_client> mqtt_client
) {

    this->mqtt_client = mqtt_client;

    this->config = config;

    json::object input_config = 
        val<json::object>(get_input_config_name(), config);
    input_topic = val<string>("topic", input_config);
    input_message_type = val<string>("message_type", input_config);
    input_sub_type = val<string>("sub_type", input_config);

    json::object output_config = 
        val<json::object>(get_output_config_name(), config);
    output_topic = val<string>("topic", output_config);
    output_message_type = val<string>("message_type", output_config);
    output_sub_type = val<string>("sub_type", output_config);
    version = val<string>("version", config);
    source = val<string>("agent_name", config);
}


// Called by the MQTT client when traffic is received on a subscribed topic
void MessageHandler::process_message(
    string message_topic,
    mqtt::const_message_ptr message_ptr
){
    if(!running) {
        return;
    }

    /* topic must match our configuration */
    if(input_topic.compare(message_topic)) {
        return;
    }

    /* input message must be valid JSON  */
    json_parser.reset();
    error_code ec;
    json_parser.write(message_ptr->get_payload_str(), ec);
    if(ec) {
        cerr << "Error reading form topic: " << message_topic << endl;
	cerr << "Message is not valid JSON." << endl;
        cerr << "JSON parse error code: " << ec << endl;
        return;
    }

    // at this point the message is on our topic and valid JSON
    json::object input_message = 
        json::value_to<json::object>(json_parser.release());

    process_message(input_message);
}

void MessageHandler::process_message(json::object input_message) {
    process_header(input_message, json::object(), get_timestamp());
}

void MessageHandler::process_header(
    json::object input_message,
    json::object output_message,
    string timestamp
){
    json::object input_header = 
        val<json::object>("header", input_message);

    if(!valid_input_header(input_header)){
        return;
    }

    // if the input header has no testbed version number, default to 1.0
    string version_maybe = val<string>("version", input_header);
    string testbed_version = version_maybe.empty()? "1.0": version_maybe;

    json::object header;
    header["timestamp"] = timestamp;
    header["message_type"] = output_message_type;
    header["version"] = testbed_version;

    output_message["header"] = header;

    process_msg(input_message, output_message, timestamp);
}

// true if header.message_type matches our configuration
bool MessageHandler::valid_input_header(json::object input_header) {
    string message_type = val<string>("message_type", input_header);
    return (input_message_type.compare(message_type) == 0);
}

// true if msg.sub_type matches our configuration
bool MessageHandler::valid_input_msg(json::object input_msg) {
    string sub_type = val<string>("sub_type", input_msg);
    return (input_sub_type.compare(sub_type) == 0);
}


void MessageHandler::process_msg(
    json::object input_message,
    json::object output_message,
    string timestamp
){
    // msg.sub_type must match our configuration
    json::object input_msg = 
        val<json::object>("msg", input_message);

    if(!valid_input_msg(input_msg)) {
        return;
    }

    json::object msg;
    msg["source"] = source;
    msg["version"] = version;
    msg["timestamp"] = timestamp;
    
    // add these fields if they exist in the input and have values
    string experiment_id = val<string>("experiment_id", input_msg);
    if(!experiment_id.empty()) {
        msg["experiment_id"] = experiment_id;
    }
    string trial_id = val<string>("trial_id", input_msg);
    if(!trial_id.empty()) {
        msg["trial_id"] = trial_id;
    }
    string replay_root_id = val<string>("replay_root_id", input_msg);
    if(!replay_root_id.empty()) {
        msg["replay_root_id"] = replay_root_id;
    }
    string replay_id = val<string>("replay_id", input_msg);
    if(!replay_id.empty()) {
        msg["replay_id"] = replay_id;
    }

    output_message["msg"] = msg;

    process_data(input_message, output_message);
}

void MessageHandler::process_data(
    json::object input_message,
    json::object output_message
){
    json::object input_data = 
        val<json::object>("data", input_message);

    output_message["data"] = get_data(input_data);

    publish(output_message);
}

void MessageHandler::publish(json::value jv) {
    mqtt_client->publish(output_topic, json::serialize(jv));
}
