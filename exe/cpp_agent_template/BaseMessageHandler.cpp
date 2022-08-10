#include "BaseMessageHandler.hpp"
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>

using namespace std;
namespace json = boost::json;


BaseMessageHandler::BaseMessageHandler(string name, json::object config):
    Message(name, config)
{

}

