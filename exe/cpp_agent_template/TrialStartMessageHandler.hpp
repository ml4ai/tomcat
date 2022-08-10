#pragma once

#include <string>
#include <boost/json.hpp>
#include "BaseMessageHandler.hpp"


namespace json = boost::json;

using namespace std;


/** trial message */
class TrialStartMessageHandler: public BaseMessageHandler {

    public:

    /** Constructor */
    TrialStartMessageHandler(json::object config);

    /** handler call */
    void handle_message(string topic, json::object message);
};
