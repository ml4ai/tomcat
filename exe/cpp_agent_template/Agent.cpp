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

    // advise of subscribed topics
    std::cout << "Subscription topics:" << std::endl;
    for (std::string topic : processor.get_input_topics()) {
        std::cout << "\t" << topic << std::endl;
    }

    // advise of published topics
    std::cout << "Publication topics:" << std::endl;
    for (std::string topic : processor.get_output_topics()) {
        std::cout << "\t" << topic << std::endl;
    }
}

void Agent::log_subscription_activity(const std::string topic){
    subscription_activity.push_back(topic);
}

void Agent::log_publication_activity(const std::string topic){
    publication_activity.push_back(topic);
}

// Report the activity on the subscription and publication topics
void Agent::summarize_activity() {
    std::cout << "Messages subscribed: ";
    std::cout << subscription_activity.size() << std::endl;
    count_keys(subscription_activity);
    std::cout << "Messages published: ";
    std::cout << publication_activity.size() << std::endl;
    count_keys(publication_activity);
}

// return JSON parsed from input or empty object if not valid JSON
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
