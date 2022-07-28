#pragma once

#include "Agent.hpp"

using namespace std;

class ReferenceAgent : public Agent {
    void process(mqtt::const_message_ptr msg) override;

  public:
    ReferenceAgent(string address, string input_topic, string output_topic);
};
