#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "ReferenceMessageHandler.hpp"


using namespace std;
namespace json = boost::json;

// Set parameters using the config file input.  Runs once at startup
void ReferenceMessageHandler::configure(
    json::object config,
    std::shared_ptr<mqtt::async_client> mqtt_client
) {

    MessageHandler::configure(config, mqtt_client);

    // add anything needed from the config file
}

// return data for the subscribed input
json::object ReferenceMessageHandler::get_data(json::object input_data) {

    // add anything needed from the subscribed input data

    data["reference_agent_data"] = "goes here";

    return data;
}
