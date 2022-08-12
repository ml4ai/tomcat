#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <mqtt/async_client.h>

#include "Processor.hpp"

using namespace std;
namespace json = boost::json;


/** Read the the topic, header.message_type, and msg.sub_type fields
 *  from the configuration.  The processor can't run without these 
 *  values */
void Processor::configure(json::object config){
    // use the configuration object that matches our hard-coded name
    string name = get_name();
    if(config.contains(name)) {
        json::object info = json::value_to<json::object>(config.at(name));
	if(info.contains("topic")) {
            topic = json::value_to<string>(info.at("topic"));
	} else {
	    cout << name << " configuration missing 'topic' field " << endl;
	    exit(EXIT_FAILURE);
	}

	if(info.contains("message_type")) {
            message_type = json::value_to<string>(info.at("message_type"));
	} else {
	    cout << name << " configuration missing 'message_type' field " << endl;
	    exit(EXIT_FAILURE);
	}

	if(info.contains("sub_type")) {
            sub_type = json::value_to<string>(info.at("sub_type"));
	} else {
	    cout << name << " configuration missing 'sub_type' field " << endl;
	    exit(EXIT_FAILURE);
	}

    } else {
        cout << "Config object does not have entry for '" << name << "'" << endl;
	exit(EXIT_FAILURE);
    }

    cout << "CONFIGURATION:" << endl;
    cout << " name:         " << name << endl;
    cout << " topic:        " << topic << endl;
    cout << " message_type: " << message_type << endl;
    cout << " sub_type:     " << sub_type << endl;
}


/** Check that the message is valid json, has the header, msg, and data objects
 *  and that the topic, message_type and sub_type match our configuration.  If
 *  everything checks out, respond to the message */
void Processor::process(mqtt::const_message_ptr m_ptr){

    /* test that the topic matches the configuration */
    string m_topic = m_ptr->get_topic();
    if(m_topic.compare(topic) != 0) {
        return;
    }

    /* convert the message to a JSON object  */
    json_parser.reset();
    error_code ec;
    json_parser.write(m_ptr->get_payload_str(), ec);
    if(ec) {
        cout << "msg.topic: " << m_topic << endl;
        cout << "JSON parse error code: " << ec << endl;
 	return;
    }
    json::object message = json::value_to<json::object>(json_parser.release());
   
    /* get the message header object */
    if(!message.contains("header")) {
        cout << "Message has no header object" << endl;
        return;
    }
    json::object header = json::value_to<json::object>(message.at("header"));

    /* test that the header message_type matches the configuration */
    if(!test_key_value(header, "message_type", message_type)) {
	return;
    }

    /* get the msg object and test that it is ours */
    if(!message.contains("msg")) {
        cout << "Message has no msg object" << endl;
        return;
    }
    json::object msg = json::value_to<json::object>(message.at("msg"));

    /* test that the msg sub_type matches the configuration */
    if(!test_key_value(msg, "sub_type", sub_type)) {
	return;
    }

    /* get the data object */
    if(!message.contains("data")) {
        cout << "Message has no data object" << endl;
        return;
    }
    json::object data = json::value_to<json::object>(message.at("data"));

    /* If all the message objects exist and match the configuration, process
     * the JSON message */
    process(message);
}


/* test for matching key value in a json object */
bool Processor::test_key_value(json::object obj, string key, string value) {
    /* test that key exists */
    if(!obj.contains(key)) {
        return false;
    }

    /* test value */
    string keyval = json::value_to<string>(obj.at(key));

    if(value.compare(keyval) == 0) {
        return true;
    }

    return false;
}



