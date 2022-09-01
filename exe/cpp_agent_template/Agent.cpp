#include "Agent.hpp"
#include "ReferenceProcessor.hpp"
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <iostream>

namespace json = boost::json;

void Agent::configure(const json::object& config) {

    reference_processor.configure(config);

    std::string agent_name = val<std::string>(config, "agent_name");
    std::string version = val<std::string>(config, "version", "1.0.0");

    app_name = agent_name + " version " + version;
    std::cout << app_name << " initializing ..." << std::endl;

    // advise of subscribed topics
    std::cout << "Subscription topics:" << std::endl;
    for (std::string i : reference_processor.get_input_topics()) {
        std::cout << "\t" << i << std::endl;
    }

    // advise of published topics
    std::cout << "Publication topics:" << std::endl;
    for (std::string i : reference_processor.get_output_topics()) {
        std::cout << "\t" << i << std::endl;
    }
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
