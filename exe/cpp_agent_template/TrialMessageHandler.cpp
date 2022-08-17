#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "TrialMessageHandler.hpp"

using namespace std;
namespace json = boost::json;


void TrialMessageHandler::configure(
    json::object config,
    std::shared_ptr<mqtt::async_client> mqtt_client
) {

    MessageHandler::configure(config, mqtt_client);    

    // static data never changes
    data["agent_name"] = get_value<string>("agent_name", config);
    data["owner"] = get_value<string>("owner", config);
    data["version"] = get_value<string>("version", config);
    data["source"] = get_value<json::array>("source", config);
    data["config"] = get_value<json::array>("config", config);
    data["dependencies"] = 
        get_value<json::array>("dependencies", config);
    data["publishes"] = {
        get_value<json::object>("reference_agent_output", config),
        get_value<json::object>("rollcall_response", config),
        get_value<json::object>("heartbeat", config),
        get_value<json::object>("version_info", config)
    };
    data["subscribes"] = {
        get_value<json::object>("reference_agent_input", config),
        get_value<json::object>("rollcall_request", config),
        get_value<json::object>("trial_start", config),
        get_value<json::object>("trial_stop", config)
    };
}

