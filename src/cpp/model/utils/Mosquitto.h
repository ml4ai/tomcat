/**
 * Based on:
 * Mosquitto C++ Wrapper
 * (C) 2017, Felix Niederwanger
 * MIT License
 */

#pragma once

#include <chrono>
#include <sstream>
#include <string>

#include <mosquitto.h>

#include "Definitions.h"

/**
 * A wrapper for the mosquitto C library
 */
class Mosquitto {
  public:
    //------------------------------------------------------------------
    // Types, Enums & Constants
    //------------------------------------------------------------------
    typedef std::chrono::steady_clock::time_point Time;

    //--------------------------------------------------------------------------
    // Constructors & Destructor
    //--------------------------------------------------------------------------

    /**
     * Creates an instance of the wrapper.
     */
    Mosquitto();

    virtual ~Mosquitto();

    //--------------------------------------------------------------------------
    // Copy & Move constructors/assignments
    //--------------------------------------------------------------------------
    Mosquitto(const Mosquitto& mosquitto);

    Mosquitto& operator=(const Mosquitto& mosquitto);

    Mosquitto(Mosquitto&&) = default;

    Mosquitto& operator=(Mosquitto&&) = default;

    //--------------------------------------------------------------------------
    // Member functions
    //--------------------------------------------------------------------------

    /**
     * Connects the client to the broker.
     *
     * @param address: remote broker address
     * @param port: remote broker port
     * @param keep_alive_for: delay in seconds for pings for the connection to
     * stay alive
     */
    void connect(const std::string& address, int port, int alive_delay);

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
     * Loop through messages. This call usually blocks until the connection is
     * closed.
     *
     * @param try_reconnect: indicates whether the client must try to reconnect
     * to the broker in case of unexpected disconnection
     */
    void loop(const bool try_reconnect = true);

    /**
     * Closes the client's connection.
     */
    void close();

    //--------------------------------------------------------------------------
    // Getters & Setters
    //--------------------------------------------------------------------------
    void set_max_seconds_without_messages(long max_seconds_without_messages);

  protected:
    //--------------------------------------------------------------------------
    // Virtual functions
    //--------------------------------------------------------------------------

    /**
     * Copies the data members of another instance of this wrapper.
     */
    void copy_wrapper(const Mosquitto& mosquitto);

    /**
     * Function called when the client is connected.
     */
    virtual void on_connected();

    /**
     * Function called when an error occurred.
     * @param msg
     */
    virtual void on_error(const std::string& error_message);

    /**
     * Function called when a new message arrives.
     *
     * @param topic: message's topic
     * @param message: message's content
     */
    virtual void on_message(const std::string& topic,
                            const std::string& message);

    /**
     * Function called when the client is enough time without receiving a new
     * message.
     */
    virtual void on_time_out();

    //--------------------------------------------------------------------------
    // Data members
    //--------------------------------------------------------------------------
    volatile bool running;

  private:
    //--------------------------------------------------------------------------
    // Static functions
    //--------------------------------------------------------------------------

    /**
     * Callback function called upon the establishment of connection with the
     * broker.
     *
     * @param mqtt_client: client instance
     * @param wrapper_instance: instance of this class
     * @param error_code: error code
     */
    static void mosquitto_callback_on_connect(struct mosquitto* mqtt_client,
                                              void* wrapper_instance,
                                              int error_code);

    /**
     * Callback function called upon message arrivals.
     *
     * @param mqtt_client: client instance
     * @param wrapper_instance: instance of this class
     * @param message: message's topic and payload
     */
    static void
    mosquitto_callback_on_message(struct mosquitto* mqtt_client,
                                  void* wrapper_instance,
                                  const struct mosquitto_message* message);

    //--------------------------------------------------------------------------
    // Member functions
    //--------------------------------------------------------------------------

    /**
     * Checks if the client is enough time without receiving messages.
     *
     * @return Whether the seconds with no messages received is greater than the
     * maximum allowed.
     */
    bool time_out() const;

    //--------------------------------------------------------------------------
    // Data members
    //--------------------------------------------------------------------------
    struct mosquitto* mqtt_client;

    // Time at the last time a message arrived.
    Time last_updated_time;

    long max_seconds_without_messages;
};
