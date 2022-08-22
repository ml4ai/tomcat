#pragma once

#include <boost/json.hpp>
#include "Agent.hpp"

namespace json = boost::json;

using namespace std;


/** Agent class that manages MQTT traffic  */
class FileAgent : public Agent {

    public:

    /* write output to the filesystem */
    void write(const string topic, json::object &message) override;
};
