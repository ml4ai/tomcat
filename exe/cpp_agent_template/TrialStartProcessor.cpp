#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "TrialStartProcessor.hpp"

using namespace std;
namespace json = boost::json;


TrialStartProcessor::TrialStartProcessor(json::object config):
    Processor("trial_start", config) 
{

}

void TrialStartProcessor::process(string topic, json::object message){
}
