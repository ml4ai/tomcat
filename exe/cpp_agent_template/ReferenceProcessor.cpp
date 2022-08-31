#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "ReferenceProcessor.hpp"
#include "Processor.hpp"
#include <boost/json/array.hpp>
#include "Agent.hpp"


namespace json = boost::json;

void ReferenceProcessor::configure(const json::object &config) {
    Processor::configure(config);	

    add_subscriptions(config);
    add_publications(config);

    config_input_topics = get_array_values(subscribes, "topic");
    config_output_topics = get_array_values(publishes, "topic");
}

// process a custom-defined message. 
void ReferenceProcessor::process_message(
    const json::object &input_message) {

    std::string input_topic = val<std::string>(input_message, "topic");

    // Process the message if subscribed to the topic
    if(contains(config_input_topics, input_topic)) {

        // publish the output message on each of the output topics
	for(auto &output_topic : config_output_topics) {

            // create a data element 
            json::object output_data;
            output_data["input_topic"] = input_topic;
            output_data["output_topic"] = output_topic;
            output_data["text"] = "Hello World!";

            publish(output_topic, input_message, output_data);
	}
    }

    // forward the message to base class for further processing
    Processor::process_message(input_message);
}
