#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "RollcallProcessor.hpp"

using namespace std;
namespace json = boost::json;


RollcallProcessor::RollcallProcessor(json::object config):
    Processor("rollcall_request", config) 
{


}

void RollcallProcessor::process(string topic, json::object message){
}

