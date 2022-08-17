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
        get_value<json::object>(get_input_config_name(), config);
    input_topic = get_value<string>("topic", input_config);
    input_message_type = get_value<string>("message_type", input_config);
    input_sub_type = get_value<string>("sub_type", input_config);

    json::object output_config = 
        get_value<json::object>(get_output_config_name(), config);
    output_topic = get_value<string>("topic", output_config);

    // 
    header["message_type"] = get_value<string>("message_type", output_config);
    msg["sub_type"] = get_value<string>("sub_type", output_config);
    msg["version"] = get_value<string>("version", config);
    msg["source"] = get_value<string>("agent_name", config);
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
    json::object json_message = 
        json::value_to<json::object>(json_parser.release());

    // compose an output message from the json message
    process_json_message(json_message);
}

// Screen the input message by our config parameters and then respond.
void MessageHandler::process_json_message(json::object json_message){

    // header.message_type must match our configuration
    json::object input_header = 
        get_value<json::object>("header", json_message);
    string message_type = get_value<string>("message_type", input_header);
    if(input_message_type.compare(message_type)) {
        return;
    }

    // either of the msg.sub_types must match our configuration
    json::object input_msg = 
        get_value<json::object>("msg", json_message);
    string sub_type = get_value<string>("sub_type", input_msg);
    if(input_sub_type.compare(sub_type)) {
        return;
    }

    json::object input_data = get_value<json::object>("data", json_message);

    // Message is our input, publish a response
    publish(get_message(input_header, input_msg, input_data));
}

// Create message for publication
json::object MessageHandler::get_message(
    json::object input_header,
    json::object input_msg,
    json::object input_data)
{
    string timestamp = get_timestamp();

    json::object message;
    message["header"] = get_header(header, input_header, timestamp);
    message["msg"] = get_msg(msg, input_header, timestamp);
    message["data"] = get_data(input_data);

    return message;
}

// load the fields of the output header object
json::object MessageHandler::get_header(
    json::object output_header,
    json::object input_header,
    string timestamp)
{
    string version_maybe = get_value<string>("version", input_header);
    string version = version_maybe.empty()? "1.0": version_maybe;

    output_header["timestamp"] = timestamp;
    output_header["version"] = version;

    return output_header;
}


// load the fields of the output msg object
json::object MessageHandler::get_msg(
    json::object output_msg,
    json::object input_msg,
    string timestamp)
{
    output_msg["timestamp"] = timestamp;

    // only include these fields if they are present in the input
    copy_or_erase(input_msg, output_msg, "experiment_id");
    copy_or_erase(input_msg, output_msg, "trial_id");
    copy_or_erase(input_msg, output_msg, "replay_root_id");
    copy_or_erase(input_msg, output_msg, "replay_id");

    return output_msg;
}

// Add the key/value pair from src into dst.  If src does not have the
// key, erase that field from dst.
void MessageHandler::copy_or_erase(
    json::object src,
    json::object dst,
    string key)
{
    if(src.contains(key)) {
        dst[key] = src.at(key);
    } else {
	dst.erase(key);
    }
}

void MessageHandler::publish(json::value jv) {
    mqtt_client->publish(output_topic, json::serialize(jv));
}
