#include "CdcProcessor.hpp"
#include "Agent.hpp"
#include "Processor.hpp"
#include <boost/json.hpp>
#include <boost/json/array.hpp>
#include <boost/log/trivial.hpp>

namespace json = boost::json;


void CdcProcessor::configure(
    const json::object& config,
    Agent *agent) {

    // add input subscriptions
    add_subscription(input_topic, input_type, input_sub_type);

    // add output to publications
    add_publication(output_topic, output_type, output_sub_type);

    Processor::configure(config, agent);
}

// process a custom-defined message.
void CdcProcessor::process_message(const json::object& input_message) {

    std::string input_topic = val<std::string>(input_message, "topic");

    // Process the message if subscribed to the topic
    if (contains(input_topics, input_topic)) {

        // publish the output message on each of the output topics
        for (auto& output_topic : output_topics) {

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
