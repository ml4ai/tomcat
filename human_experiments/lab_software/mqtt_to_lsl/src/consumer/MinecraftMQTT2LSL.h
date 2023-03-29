#pragma once

#include <string>

#include "data_stream/LSLStringStream.h"

class MinecraftMQTT2LSL {
  public:
    MinecraftMQTT2LSL();
    ~MinecraftMQTT2LSL() = default;

    MinecraftMQTT2LSL(const MinecraftMQTT2LSL&) = delete;
    MinecraftMQTT2LSL& operator=(const MinecraftMQTT2LSL&) = delete;
    MinecraftMQTT2LSL(MinecraftMQTT2LSL&&) = default;
    MinecraftMQTT2LSL& operator=(MinecraftMQTT2LSL&&) = default;

    /**
     * Starts to monitor messages and push them to LSL
     *
     */

    /**
     * Connects to MQTT broker and wait for messages.
     *
     * @param mqtt_address: address of the MQTT server
     * @param mqtt_port: port of the MQTT server
     */
    void start(const std::string& mqtt_address, int mqtt_port);

  private:
    std::unique_ptr<LSLStringStream> minecraft_lsl_stream;

    /**
     * Callback function called everytime we receive a message from a subscribed
     * topic. In this function, we add the topic to the message and push it to
     * LSL.
     *
     * @param topic: MQTT message topic
     * @param message: MQTT message
     */
    void push_to_lsl(const std::string& topic, const std::string& message);
};
