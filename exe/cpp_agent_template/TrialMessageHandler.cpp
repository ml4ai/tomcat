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

    // Version Info data.  Never changes.
    data["agent_name"] = val<string>("agent_name", config);
    data["owner"] = val<string>("owner", config);
    data["version"] = val<string>("version", config);
    data["source"] = val<json::array>("source", config);
    data["config"] = val<json::array>("config", config);
    data["dependencies"] = 
        val<json::array>("dependencies", config);
    data["publishes"] = {
        val<json::object>("reference_agent_output", config),
        val<json::object>("rollcall_response", config),
        val<json::object>("heartbeat", config),
        val<json::object>("version_info", config)
    };
    data["subscribes"] = {
        val<json::object>("reference_agent_input", config),
        val<json::object>("rollcall_request", config),
        val<json::object>("trial_start", config),
        val<json::object>("trial_stop", config)
    };
}
