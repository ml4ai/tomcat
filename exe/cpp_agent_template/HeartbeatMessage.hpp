#pragma once

#include <string>
#include <boost/json.hpp>
#include "PublishedMessage.hpp"

namespace json = boost::json;
using namespace std;


class HeartbeatMessage: public PublishedMessage {

    public:

    /** state */
    string state = "code development";  // "ok"

    /** Data unique to the HeartbeatMessage */
    json::value data_json_value() override;

    /** Constructor */
    HeartbeatMessage(json::object config);
};
