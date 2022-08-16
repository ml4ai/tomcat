#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "VersionInfoAgent.hpp"


using namespace std;
namespace json = boost::json;

json::object VersionInfoAgent::get_input_config(json::object config){
    return get_object("trial_start", config);
}    

json::object VersionInfoAgent::get_output_config(json::object config){
    return get_object("version_info", config);
}    

json::object VersionInfoAgent::get_output_data(json::object input_data){ 
    json::object output_data;
    output_data["version_info_data"] = "goes here";

    return output_data;
}
