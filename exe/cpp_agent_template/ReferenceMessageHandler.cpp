#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "ReferenceMessageHandler.hpp"
#include "Agent.hpp"


using namespace std;
namespace json = boost::json;

// Set parameters using the config file input.  Runs once at startup
void ReferenceMessageHandler::configure(json::object config, Agent *agent) {

    MessageHandler::configure(config, agent);

    // add anything needed from the config file
    data["configure_time"] = get_timestamp();
}

// return data for the subscribed input
json::object ReferenceMessageHandler::get_data(json::object input_data) {

    // add anything needed from the subscribed input data
    data["data_from_input"] = "updates with_input";

    // show persistent state 
    counter ++;
    data["counter"] = counter;

    return data;
}
