#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "RollcallAgent.hpp"


using namespace std;
namespace json = boost::json;

json::object RollcallAgent::get_input_config(json::object config){
    return get_object("rollcall_request", config);
}    

json::object RollcallAgent::get_output_config(json::object config){
    return get_object("rollcall_response", config);
}    

json::object RollcallAgent::get_output_data(json::object input_data){ 

    int uptime = 1234;  // TODO compute actual

    json::object output_data;
    output_data["rollcall_id"] = get_string("rollcall_id", input_data);
    output_data["status"] = "up";
    output_data["uptime"] = uptime;
    output_data["version"] = version;

    return output_data;
}
