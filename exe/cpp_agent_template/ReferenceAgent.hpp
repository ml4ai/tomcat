#pragma once

#include <boost/json.hpp>
#include "AgentBase.hpp"

using namespace std;
namespace json = boost::json;


class ReferenceAgent : public AgentBase {

    public:

    // constructor
    ReferenceAgent(json::object config);

    // input processor
    void process(mqtt::const_message_ptr msg) override;
};
