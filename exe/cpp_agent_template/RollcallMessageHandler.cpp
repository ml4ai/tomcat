#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "RollcallMessageHandler.hpp"


using namespace std;
namespace json = boost::json;

// Set parameters using the config file input. 
void RollcallMessageHandler::configure(
    json::object config,
    std::shared_ptr<mqtt::async_client> mqtt_client
) {

    MessageHandler::configure(config, mqtt_client);

    data["version"] = get_value<string>("version", config);
    data["status"] = "up";  // always?
}


json::object RollcallMessageHandler::get_data(json::object input_data){

    int uptime = 1234;  // TODO compute actual

    data["rollcall_id"] = get_value<string>("rollcall_id", input_data);
    data["uptime"] = uptime;

    return data;
}
