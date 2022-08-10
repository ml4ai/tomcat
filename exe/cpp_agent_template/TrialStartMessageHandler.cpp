#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "TrialStartMessageHandler.hpp"

using namespace std;
namespace json = boost::json;


TrialStartMessageHandler::TrialStartMessageHandler(json::object config):
    BaseMessageHandler("trial_start", config) 
{

}


void TrialStartMessageHandler::handle_message(string topic, json::object message){
}


