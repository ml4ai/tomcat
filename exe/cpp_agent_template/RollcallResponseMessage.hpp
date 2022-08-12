#pragma once

#include <string>
#include <boost/json.hpp>
#include "PublishedMessage.hpp"

namespace json = boost::json;
using namespace std;


class RollcallResponseMessage: public PublishedMessage {

    public:

    /** Data unique to the RollcallResponseMessage */
    json::value data_json_value() override;

    /** Constructor */
    RollcallResponseMessage(json::object config);
};
