#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "VersionInfoAgent.hpp"



using namespace std;
namespace json = boost::json;



void VersionInfoAgent::configure(
    json::object config,
    std::shared_ptr<mqtt::async_client> mqtt_client
) {

    BaseAgent::configure(config, mqtt_client);    

    output_data["agent_name"] = get_value<string>("agent_name", config);
    output_data["owner"] = get_value<string>("owner", config);
    output_data["version"] = get_value<string>("version", config);
    output_data["source"] = get_value<json::array>("source", config);
    output_data["config"] = get_value<json::array>("config", config);
    output_data["dependencies"] = 
        get_value<json::array>("dependencies", config);
    output_data["publishes"] = {
        get_value<json::object>("reference_agent_output", config),
        get_value<json::object>("rollcall_response", config),
        get_value<json::object>("heartbeat", config),
        get_value<json::object>("version_info", config)
    };
    output_data["subscribes"] = {
        get_value<json::object>("reference_agent_input", config),
        get_value<json::object>("rollcall_request", config),
        get_value<json::object>("trial_start", config),
        get_value<json::object>("trial_stop", config)
    };



}

json::object VersionInfoAgent::get_input_config(json::object config){
    return get_value<json::object>("trial_start", config);
}    

json::object VersionInfoAgent::get_output_config(json::object config){
    return get_value<json::object>("version_info", config);
}    

json::object VersionInfoAgent::create_output_data(json::object input_data){ 

    return output_data;
}
