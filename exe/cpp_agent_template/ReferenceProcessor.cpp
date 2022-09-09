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

    // add subscriptions by topic only
    json::array subs = val<json::array>(config, "subscribes");
    for (size_t i = 0; i < subs.size(); i++) {
        std::string topic = json::value_to<std::string>(subs.at(i));
        add_subscription(topic, "not_set", "not_set");
    }
    subscription_topics = get_subscription_topics();

    // add publications by topic only
    json::array pubs = val<json::array>(config, "publishes");
    for (size_t i = 0; i < pubs.size(); i++) {
        std::string topic = json::value_to<std::string>(pubs.at(i));
        add_publication(topic, "not_set", "not_set");
    }
    publication_topics = get_publication_topics();

    // configure the base class last, it will add its own topics
    Processor::configure(config, agent);
}

// process a custom-defined message.
void ReferenceProcessor::process_message(const std::string topic,
                                         const json::object& message) {


    json::object header = val<json::object>(message, "header");
    json::object msg = val<json::object>(message, "msg");

    // Process the message if subscribed to the topic
    if (contains(subscription_topics, topic)) {

        // publish to each of the publication topics
        for (auto& publication_topic : publication_topics) {

            std::string timestamp = get_timestamp();

            // create a basic message
            json::object new_message = {
                { "header", new_header(header, timestamp, "not_set") },
                { "msg", new_msg(msg, timestamp, "not_set") },
                { "data", {
                    { "subscription_topic", topic },
                    { "publication_topic", publication_topic },
                    { "text", "ReferenceProcessor says Hello World!" }}}};

            publish(publication_topic, new_message);
        }
    }

    // forward the message to base class for further processing
    Processor::process_message(topic, message);
}
