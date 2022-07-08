#include <cstdlib>
#include <iostream>
#include <mutex>
#include <signal.h>
#include <string>
#include <thread>
#include <unistd.h>

#include <mqtt/async_client.h>
#include <nlohmann/json.hpp>

void run_mqtt(string mqtt_host, string mqtt_port, string player_name,
              AudioStreamer& streamer) {
    // Connect to MQTT broker
    string server_address = "tcp://" + mqtt_host + ":" + mqtt_port;
    string client_id = player_name + "_audioStreamer";
    mqtt::async_client mqtt_client(server_address, client_id);

    auto connOpts =
        mqtt::connect_options_builder()
            .clean_session(true)
            .automatic_reconnect(chrono::seconds(2), chrono::seconds(30))
            .finalize();

    try {
        mqtt_client.set_message_callback([&](mqtt::const_message_ptr msg) {
            mqtt_process_message(msg, streamer);
        });
        mqtt_client.connect(connOpts)->get_connect_response();
        mqtt_client.subscribe("trial", 2);

        BOOST_LOG_TRIVIAL(info) << "Connection to MQTT broker successful, "
                                   "awaiting Trial Start message";

        // Consume messages
        chrono::milliseconds duration(1);
        while (!SHUTDOWN) {
            // Yield main thread until exit
            this_thread::yield();
            this_thread::sleep_for(duration);
        }

        // Shutdown stream if active
        streamer.StopStreaming(); // AudioStreamer will check if stream is
                                  // active when function is called

        // Shutdown MQTT connection
        mqtt_client.stop_consuming();
        mqtt_client.disconnect();
    }
    catch (const mqtt::exception& e) {
        BOOST_LOG_TRIVIAL(error)
            << "Failure in MQTT client. Error was: " << e.what();
        return;
    }
}

void mqtt_process_message(mqtt::const_message_ptr msg,
                          AudioStreamer& streamer) {
    string payload = msg->get_payload_str();
    nlohmann::json message = nlohmann::json::parse(payload);

    string sub_type = message["msg"]["sub_type"];
    if (sub_type == "start") {
        BOOST_LOG_TRIVIAL(info)
            << "Trial start message recieved, starting audio stream. ";
        streamer.GenerateAudioFilename(message);
        streamer.StartStreaming();
    }
    else if (sub_type == "stop") {
        BOOST_LOG_TRIVIAL(info)
            << "Trial stop message recieved, stopping audio stream. ";
        streamer.StopStreaming();
    }
}