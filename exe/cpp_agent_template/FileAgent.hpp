#pragma once

#include <memory>
#include <string>
#include <thread> // TODO needed?
#include <future> // TODO needed?
#include <boost/json.hpp>

#include <mqtt/async_client.h>

#include "Agent.hpp"

namespace json = boost::json;

using namespace std;


/** Agent class that manages MQTT traffic  */
class FileAgent : public Agent {

  public:

    /** Constructor */
    FileAgent(json::object config);

    /* send output to the message bus */
    void write(string topic, json::object message) override;
};
