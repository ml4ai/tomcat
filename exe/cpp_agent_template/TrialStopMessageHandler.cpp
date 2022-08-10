#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "TrialStopMessageHandler.hpp"

using namespace std;
namespace json = boost::json;


TrialStopMessageHandler::TrialStopMessageHandler(json::object config):
    BaseMessageHandler("trial_stop", config) 
{

}


void TrialStopMessageHandler::handle_message(string topic, json::object message){
}


