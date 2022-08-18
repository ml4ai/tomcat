#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "RollcallMessageHandler.hpp"
#include "Agent.hpp"


using namespace std;
namespace json = boost::json;

// Set parameters using the config file input. 
void RollcallMessageHandler::configure(json::object config, Agent *agent) {

    MessageHandler::configure(config, agent);

    data["version"] = val<string>("version", config);
    data["status"] = "up";  // always?
}

json::object RollcallMessageHandler::get_data(json::object input_data){

    int uptime = 1234;  // TODO compute actual

    data["rollcall_id"] = val<string>("rollcall_id", input_data);
    data["uptime"] = uptime;

    return data;
}
