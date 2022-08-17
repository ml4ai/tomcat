#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "ReferenceAgent.hpp"


using namespace std;
namespace json = boost::json;

json::object ReferenceAgent::create_output_data(json::object input_data){
    json::object output_data;
    output_data["reference_agent_data"] = "goes here";

    return output_data;
}
