#pragma once

#include "AgentBase.hpp"

using namespace std;

class ReferenceAgent : public AgentBase {
    void process(mqtt::const_message_ptr msg) override;
    string input_topic = "";
    string output_topic = "";

  public:
    ReferenceAgent(string address, string input_topic, string output_topic);
};
