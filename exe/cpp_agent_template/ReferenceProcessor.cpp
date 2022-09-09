#include "ReferenceProcessor.hpp"
#include "Agent.hpp"
#include "Processor.hpp"
#include <boost/json.hpp>
#include <boost/json/array.hpp>
#include <boost/log/trivial.hpp>

namespace json = boost::json;

void ReferenceProcessor::configure(
    const json::object& config,
    Agent *agent) {

    // add input subscriptions by topic only
    json::array subs = val<json::array>(config, "subscribes");
    for (size_t i = 0; i < subs.size(); i++) {
        std::string topic = json::value_to<std::string>(subs.at(i));
        add_subscription(topic, "not_set", "not_set");
    }
    input_topics = get_input_topics();

    // add output publications by topic only
    json::array pubs = val<json::array>(config, "publishes");
    for (size_t i = 0; i < pubs.size(); i++) {
        std::string topic = json::value_to<std::string>(pubs.at(i));
        add_publication(topic, "not_set", "not_set");
    }
    output_topics = get_output_topics();

    // now add the base class topics
    Processor::configure(config, agent);
}

// process a custom-defined message.
void ReferenceProcessor::process_message(const std::string topic,
                                         const json::object& message) {

    std::string timestamp = get_timestamp();

    json::object header = val<json::object>(message, "header");
    json::object msg = val<json::object>(message, "msg");

    // Process the message if subscribed to the topic
    if (contains(input_topics, topic)) {

        // publish the output message on each of the output topics
        for (auto& output_topic : output_topics) {

            // create a basic message
            json::object new_message = {
                { "header", new_header(header, timestamp, "not_set") },
                { "msg", new_msg(msg, timestamp, "not_set") },
                { "data", {
                    { "input_topic", topic },
                    { "output_topic", output_topic },
                    { "text", "ReferenceProcessor says Hello World!" }}}};

            publish(output_topic, new_message);
        }
    }

    // forward the message to base class for further processing
    Processor::process_message(topic, message);
}
