#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "Agent.hpp"
#include "ReferenceMessageHandler.hpp"
#include <iostream>

using namespace std;
namespace json = boost::json;

void Agent::configure(const json::object &config) {

    message_handler.configure(config);

    string agent_name = val<string>(config, "agent_name");
    string version = val<string>(config, "version", "1.0.0");

    app_name = agent_name + " version " + version;
    cout << app_name << " initializing ..." << endl;

    // advise of subscribed topics
    cout << "Subscription topics:" << endl;
    for(string i : message_handler.get_input_topics()) {
        cout << "    " << i << endl;
    }

    // advise of published topics
    cout << "Publication topics:" << endl;
    for(string i : message_handler.get_output_topics()) {
        cout << "    " << i << endl;
    }
}

// return JSON parsed from input or empty object if not valid JSON
json::object Agent::parse_json(const string text) {
    json_parser.reset();
    error_code ec;
    json_parser.write(text, ec);

    // report error
    if(ec) {
        cerr << "Error parsing JSON: " << text << endl;
        cerr << "JSON parse error code: " << ec << endl;
        return json::object();
    }

    return json::value_to<json::object>(json_parser.release());
}
