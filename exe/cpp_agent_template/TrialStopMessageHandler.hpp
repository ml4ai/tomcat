#pragma once

#include <string>
#include <boost/json.hpp>
#include "BaseMessageHandler.hpp"


namespace json = boost::json;

using namespace std;


/** trial message */
class TrialStopMessageHandler: public BaseMessageHandler {

    public:

    /** Constructor */
    TrialStopMessageHandler(json::object config);

    /** handler call */
    void handle_message(string topic, json::object message);
};
