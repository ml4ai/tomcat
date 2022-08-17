#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "RollcallAgent.hpp"


using namespace std;
namespace json = boost::json;

json::object RollcallAgent::create_output_data(json::object input_data){ 

    int uptime = 1234;  // TODO compute actual

    json::object output_data;
    output_data["rollcall_id"] = get_value<string>("rollcall_id", input_data);
    output_data["status"] = "up";
    output_data["uptime"] = uptime;
    output_data["version"] = version;

    return output_data;
}
