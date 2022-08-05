#pragma once

#include "AgentBase.hpp"

using namespace std;

class ReferenceAgent : public AgentBase {
    string input_topic = "";
    string output_topic = "";

    public:

    // constructor
    ReferenceAgent(
        string host, 
	int port,
	string input_topic,
       	string output_topic
    );

    // input processor
    void process(mqtt::const_message_ptr msg) override;
};
