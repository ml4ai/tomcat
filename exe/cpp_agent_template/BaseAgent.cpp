#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <boost/json/array.hpp>
#include <mqtt/async_client.h>
#include <boost/date_time/posix_time/posix_time.hpp>


#include "BaseAgent.hpp"
#include "Coordinator.hpp"

using namespace std;
namespace json = boost::json;

/** Get current UTC timestamp in ISO-8601 format. */
string BaseAgent::get_timestamp() {
    return boost::posix_time::to_iso_extended_string(
               boost::posix_time::microsec_clock::universal_time()) +
           "Z";
}


/** Read the the topic, header.message_type, and msg.sub_type fields
 *  from the configuration.  The processor can't run without these 
 *  values */
void BaseAgent::configure(
    json::object config,
    std::shared_ptr<mqtt::async_client> mqtt_client
) {

    this->mqtt_client = mqtt_client;

    this->config = config;

    json::object input_config = get_input_config(config);
    input_topic = get_value<string>("topic", input_config);
    input_message_type = get_value<string>("message_type", input_config);
    input_sub_type = get_value<string>("sub_type", input_config);

    json::object output_config = get_output_config(config);
    output_topic = get_value<string>("topic", output_config);
    output_message_type = get_value<string>("message_type", output_config);
    output_sub_type = get_value<string>("sub_type", output_config);

    // this software version
    version = get_value<string>("version", config);

    // this software publication source 
    source = get_value<string>("agent_name", config);
}


// Called by the MQTT client when traffic is received on a subscribed topic
void BaseAgent::process_message(
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

// called when we know the message is valid json
void BaseAgent::process_json_message(json::object json_message){

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

    // At this point we know the message is our input 
    json::object output_message = create_output_message(
        input_header,
	input_msg,
	input_data
    );

    // publish the output message
    publish(output_message);
}

// Respond to the input message
json::object BaseAgent::create_output_message(
    json::object input_header,
    json::object input_msg,
    json::object input_data)
{
    // compose response to the input message
    string timestamp = get_timestamp();

    json::object output_message;
    output_message["header"] = create_output_header(timestamp, input_header);
    output_message["msg"] = create_output_msg(timestamp, input_msg);
    output_message["data"] = create_output_data(input_data); // agent specific

    return output_message; // ready to publish
}

// create an output message common header object
json::object BaseAgent::create_output_header(
    string timestamp,
    json::object input_header) 
{
    string testbed_version = get_value<string>("version", input_header);

    json::object header;
    header["timestamp"] = timestamp;
    header["message_type"] = output_message_type;
    header["version"] = testbed_version.empty()? "1.0": testbed_version;

    return header;
}


// create an output message common msg object
json::object BaseAgent::create_output_msg(
        string timestamp, 
        json::object input_msg
) {
    json::object output_msg;
    output_msg["timestamp"] = timestamp;
    output_msg["source"] = source;
    output_msg["sub_type"] = output_sub_type;
    output_msg["version"] = version;

    // only include these fields if they are present in the input
    if(input_msg.contains("experiment_id")) {
        output_msg["experiment_id"] = input_msg.at("experiment_id");
    }
    if(input_msg.contains("trial_id")) {
        output_msg["trial_id"] = input_msg.at("trial_id");
    }
    if(input_msg.contains("replay_root_id")) {
        output_msg["replay_root_id"] = input_msg.at("replay_root_id");
    }
    if(input_msg.contains("replay_id")) {
        output_msg["replay_id"] = input_msg.at("replay_id");
    }

    return output_msg;
}

void BaseAgent::publish(json::value jv) {
    mqtt_client->publish(output_topic, json::serialize(jv));
}

// extending class overrides 
json::object BaseAgent::get_input_config(json::object config) {
    return json::object();
}
json::object BaseAgent::get_output_config(json::object config) {
    return json::object();
}
json::object BaseAgent::create_output_data(json::object input_data) {
    return json::object();
}
