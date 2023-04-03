/**
 * Based on:
 * Mosquitto C++ Wrapper
 * (C) 2017, Felix Niederwanger
 * MIT License
 */

#pragma once

#include <string>
#include <thread>
#include <functional>

#include <mosquitto.h>

/**
 * A wrapper for the mosquitto C library
 */
class Mosquitto {
  public:
    bool running = false;

    Mosquitto();
    ~Mosquitto();

    Mosquitto(const Mosquitto&) = delete;
    Mosquitto& operator=(const Mosquitto&) = delete;
    Mosquitto(Mosquitto&&) = default;
    Mosquitto& operator=(Mosquitto&&) = default;

    /**
     * Connects the client to the broker.
     *
     * @param address: remote broker address
     * @param port: remote broker port
     * @param num_trials: number of attempts to establish connection
     */
    void connect(const std::string& address, int port, int num_trials = 3);

    /**
     * Subscribes to a topic.
     *
     * @param topic: message's topic
     */
    void subscribe(const std::string& topic);

    /**
     * Publishes a message to a topic.
     *
     * @param topic: message's topic
     * @param message: message's content
     */
    void publish(const std::string& topic, const std::string& message);

    /**
     * Starts watching for messages.
     */
    void start();

    /**
     * Closes the client's connection.
     */
    void stop();

    /**
     * Sets a function to be called when messages from the subscribed topics
     * arrive.
     *
     * @param callback_fn: function to be called with parameters const
     * std::string topic, const std::string message
     */
    void set_on_message_callback(
        std::function<void(const std::string&, const std::string&)>
            callback_fn);

  private:
    struct mosquitto* mqtt_client;
    std::thread loop_thread;

    std::function<void(const std::string&, const std::string&)>
        on_message_external_callback{};

    /**
     * Callback function called upon message arrivals.
     *
     * @param mqtt_client: client instance
     * @param wrapper_instance: instance of this class
     * @param message: message's topic and payload
     */
    static void on_message_callback(struct mosquitto* mqtt_client,
                                    void* wrapper_instance,
                                    const struct mosquitto_message* message);

    /**
     * Loop until interruption.
     *
     */
    void loop();
};
