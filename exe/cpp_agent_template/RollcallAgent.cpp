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
    json::object output_data;
    output_data["rollcall_response_data"] = "goes here";

    return output_data;
}
