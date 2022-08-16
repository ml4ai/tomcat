#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <boost/json/array.hpp>
#include <mqtt/async_client.h>

#include "BaseAgent.hpp"
#include "Coordinator.hpp"
#include "Utils.hpp"

using namespace std;
namespace json = boost::json;


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
    input_topic = get_string("topic", input_config);
    input_message_type = get_string("message_type", input_config);
    input_sub_type = get_string("sub_type", input_config);

    json::object output_config = get_output_config(config);
    output_topic = get_string("topic", output_config);
    output_message_type = get_string("message_type", output_config);
    output_sub_type = get_string("sub_type", output_config);

    // this software version
    version = get_string("version", config);

    // this software publication source 
    source = get_string("publication_source", config);
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
    process_json_message(
        json::value_to<json::object>(json_parser.release())
    );
}

// called when we know the message is valid json
void BaseAgent::process_json_message(json::object json_message){

    // header.message_type must match our configuration
    json::object input_header = get_object("header", json_message);
    string message_type = get_string("message_type", input_header);
    if(input_message_type.compare(message_type)) {
        return;
    }

    // either of the msg.sub_types must match our configuration
    json::object input_msg = get_object("msg", json_message);
    string sub_type = get_string("sub_type", input_msg);
    if(input_sub_type.compare(sub_type)) {
        return;
    }

    json::object input_data = get_object("data", json_message);

    // At this point we know the message is our input 
    process_input_message(input_header, input_msg, input_data);    
}

// Respond to the input message
void BaseAgent::process_input_message(
    json::object input_header,
    json::object input_msg,
    json::object input_data)
{
    // compose response to the input message
    string timestamp = get_timestamp();
    json::value output_message = {
        {"header",
            get_output_header(timestamp, input_header)},
        {"msg",
            get_output_msg(timestamp, input_msg)},
        {"data", 
	    get_output_data(input_data)}  // agent specific
    };

    // publish output message to the Message Bus
    publish(output_topic, output_message);
}


// create an output message common header
json::object BaseAgent::get_output_header(
    string timestamp,
    json::object input_header) 
{
    string testbed_version = get_string("version", input_header);

    json::object header;
    header["timestamp"] = timestamp;
    header["message_type"] = output_message_type;
    header["version"] = testbed_version.empty()? "1.0": testbed_version;

    return header;
}


// create an output message common msg
json::object BaseAgent::get_output_msg(
        string timestamp, 
        json::object input_msg
) {
    json::object msg;
    msg["timestamp"] = timestamp;
    msg["source"] = source;
    msg["sub_type"] = output_sub_type;
    msg["version"] = version;

    // msg fields that may or may not be present
    if(input_msg.contains("experiment_id")) {
        msg["experiment_id"] = input_msg.at("experiment_id");
    }
    if(input_msg.contains("trial_id")) {
        msg["trial_id"] = input_msg.at("trial_id");
    }
    if(input_msg.contains("replay_root_id")) {
        msg["replay_root_id"] = input_msg.at("replay_root_id");
    }
    if(input_msg.contains("replay_id")) {
        msg["replay_id"] = input_msg.at("replay_id");
    }

    return msg;
}

void BaseAgent::publish(string topic, json::value jv) {
    cout << "Publishing on " << topic << endl;
    mqtt_client->publish(topic, json::serialize(jv));
}

// extending class overrides 
json::object BaseAgent::get_input_config(json::object config) {
    return json::object();
}
json::object BaseAgent::get_output_config(json::object config) {
    return json::object();
}
json::object BaseAgent::get_output_data(json::object input_data) {
    return json::object();
}
