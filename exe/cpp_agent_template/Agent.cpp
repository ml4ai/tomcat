#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "Agent.hpp"
#include "BaseMessageHandler.hpp"
#include <iostream>


// This class :

using namespace std;
namespace json = boost::json;

Agent::Agent(const json::object &config) {

    message_handler = BaseMessageHandler(this, config);
}

