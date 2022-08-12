#pragma once

#include <string>
#include <boost/json.hpp>
#include "PublishedMessage.hpp"

namespace json = boost::json;
using namespace std;


class VersionInfoMessage: public PublishedMessage {

    public:

    /** Data unique to the VersionInfoMessage */
    json::value data_json_value() override;

    /** Constructor */
    VersionInfoMessage(json::object config);
};
