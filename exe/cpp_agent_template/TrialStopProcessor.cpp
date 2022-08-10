#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "TrialStopProcessor.hpp"

using namespace std;
namespace json = boost::json;


TrialStopProcessor::TrialStopProcessor(json::object config):
    Processor("trial_stop", config) 
{

}


void TrialStopProcessor::handle_message(string topic, json::object message){
}


