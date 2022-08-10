#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "RollcallMessageHandler.hpp"

using namespace std;
namespace json = boost::json;


RollcallMessageHandler::RollcallMessageHandler(json::object config):
    BaseMessageHandler("rollcall_request", config) 
{


}

void RollcallMessageHandler::handle_message(string topic, json::object message){
}

