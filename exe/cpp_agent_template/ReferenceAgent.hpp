#pragma once

#include <boost/json.hpp>
#include "BaseAgent.hpp"

using namespace std;
namespace json = boost::json;


class ReferenceAgent : public BaseAgent {

    public:

    // constructor
    ReferenceAgent(json::object config);

    // input processor
    void process(mqtt::const_message_ptr msg) override;
};
