#pragma once

#include <string>
#include <boost/json.hpp>
#include "PublishedMessage.hpp"

namespace json = boost::json;
using namespace std;


class ReferenceAgentMessage: public PublishedMessage {

    public:

    /** Data unique to the ReferenceAgentMessage */
    json::value data_json_value() override;

    /** Constructor */
    ReferenceAgentMessage(json::object config);
};
