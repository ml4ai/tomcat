#pragma once

#include <string>
#include <boost/json.hpp>
#include "Message.hpp"


namespace json = boost::json;

using namespace std;


/** trial message */
class HeartbeatMessage: public Message {

    public:

    /** state */
    string state = "ok";

    /** publication source */
    string source;

    /** Constructor */
    HeartbeatMessage(json::object config);

    /** JSON serialization */
    json::value to_json_value(string timestamp);
    
};
