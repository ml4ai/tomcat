#pragma once

#include <memory>
#include <string>
#include <thread>
#include <future>
#include <boost/json.hpp>

#include <mqtt/async_client.h>

#include "Processor.hpp"
#include "HeartbeatMessage.hpp"
#include "ReferenceProcessor.hpp"
#include "RollcallProcessor.hpp"
#include "TrialStartProcessor.hpp"
#include "TrialStopProcessor.hpp"

namespace json = boost::json;

using namespace std;


/** Class that handles general tasks */
class Coordinator {

    /** Processors for subscribed message topics */
    TrialStartProcessor trial_start_processor;
    TrialStopProcessor trial_stop_processor;
    ReferenceProcessor reference_processor;
    RollcallProcessor rollcall_processor;

    static const int N_PROCESSORS = 4;
    Processor *processors[N_PROCESSORS] = {
        &trial_start_processor,
        &trial_stop_processor,
        &reference_processor,
        &rollcall_processor
    };

    /** config state */
    json::object config;

    /** published heartbeat */
    HeartbeatMessage *heartbeat = nullptr;

    /** Flag to specify whether the agent is running or not */
    bool running = true;

    /** std::future object holds the result of the async heartbeat operation
     */
    std::future<void> heartbeat_future;

    /** Function that processes incoming messages */
    virtual void process(mqtt::const_message_ptr msg) {};

    /** Function that publishes heartbeat messages while the agent is running */
    void publish_heartbeats();


  public:
    std::shared_ptr<mqtt::async_client> mqtt_client;

    /** Constructor */
    Coordinator(json::object config);

    /** Destructor */
    ~Coordinator();

    /** Stop the agent */
    void stop();
};
