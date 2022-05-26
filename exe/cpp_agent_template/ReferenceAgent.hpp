#pragma once

#include "Agent.hpp"

class ReferenceAgent : public Agent {
    void process(mqtt::const_message_ptr msg) override;

  public:
    ReferenceAgent(std::string address);
};
