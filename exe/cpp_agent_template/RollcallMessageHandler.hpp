#pragma once

#include <string>
#include <boost/json.hpp>
#include "BaseMessageHandler.hpp"


namespace json = boost::json;

using namespace std;


/** trial message */
class RollcallMessageHandler: public BaseMessageHandler {

    public:

    /** Constructor */
    RollcallMessageHandler(json::object config);
    
    /** handle message */
    void handle_message(string topic, json::object message);
};
