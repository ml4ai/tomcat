#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "ReferenceProcessor.hpp"

using namespace std;
namespace json = boost::json;


ReferenceProcessor::ReferenceProcessor(json::object config):
    Processor("reference_agent_input", config) 
{


}

void ReferenceProcessor::process(string topic, json::object message){
}
