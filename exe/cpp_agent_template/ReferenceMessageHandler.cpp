#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "ReferenceMessageHandler.hpp"
#include "BaseMessageHandler.hpp"
#include <boost/json/array.hpp>
#include "Agent.hpp"


namespace json = boost::json;

void ReferenceMessageHandler::configure(const json::object &config) {
    BaseMessageHandler::configure(config);	

    input_topics = add_subscriptions(config);
    output_topics = add_publications(config);
}

// process a custom-defined message. 
void ReferenceMessageHandler::process_message(
    const json::object &input_message) {

    std::string input_topic = val<std::string>(input_message, "topic");

    // Process the message if subscribed to the topic
    if(contains(input_topics, input_topic)) {

        // publish the output message on each of the output topics
	for(auto &output_topic : output_topics) {

            // create a data element 
            json::object output_data;
            output_data["input_topic"] = input_topic;
            output_data["output_topic"] = output_topic;
            output_data["text"] = "Hello World!";

            publish(output_topic, input_message, output_data);
	}
    }

    // forward the message to base class for further processing
    BaseMessageHandler::process_message(input_message);
}
