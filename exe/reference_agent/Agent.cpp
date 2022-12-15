#include "Agent.hpp"
#include "Processor.hpp"
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <iostream>

namespace json = boost::json;

void Agent::configure(const json::object& config) {
    processor.configure(config, this);

    std::string agent_name = val<std::string>(config, "agent_name");
    std::string version = val<std::string>(config, "version", "1.0.0");

    app_name = agent_name + " version " + version;
    std::cout << app_name << " initializing ..." << std::endl;

    // Every topic from which a message may be processed
    std::cout << "Subscription topics:" << std::endl;
    for (std::string topic : processor.get_subscription_topics()) {
        std::cout << "\t" << topic << std::endl;
    }

    // Every topic to which a message may be published
    std::cout << "Publication topics:" << std::endl;
    for (std::string topic : processor.get_publication_topics()) {
        std::cout << "\t" << topic << std::endl;
    }
}

void Agent::process(const std::string topic, const json::object& message) {
    std::cout << "Processing " << topic << "... " << std::endl;
    processor.process_message(topic, message);
    log_processed_topic(topic);
}

// record topic each time we process a message
void Agent::log_processed_topic(const std::string topic){
    processed_topics.push_back(topic);
}

// record topic each time we publish a message
void Agent::log_published_topic(const std::string topic){
    std::cout << "  Published " << topic << std::endl;
    published_topics.push_back(topic);
}

// Report statistics on the subscription and publication topics
void Agent::summarize_activity() {
    std::cout << "Messages processed: ";
    std::cout << processed_topics.size() << std::endl;
    count_keys(processed_topics);
    std::cout << "Messages published: ";
    std::cout << published_topics.size() << std::endl;
    count_keys(published_topics);
}

// return JSON parsed from text or empty object if not valid JSON
json::object Agent::parse_json(const std::string text) {
    json_parser.reset();
    json::error_code ec;
    json_parser.write(text, ec);

    // report error
    if (ec) {
        std::cerr << "Error parsing JSON: " << text << std::endl;
        std::cerr << "JSON parse error code: " << ec << std::endl;
        return json::object();
    }

    return json::value_to<json::object>(json_parser.release());
}
