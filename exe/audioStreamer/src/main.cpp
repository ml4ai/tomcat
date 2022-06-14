#include <cstdlib>
#include <iostream>
#include <mutex>
#include <signal.h>
#include <string>
#include <thread>
#include <unistd.h>

#include <mqtt/async_client.h>
#include <nlohmann/json.hpp>

#include <boost/asio/connect.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/websocket.hpp>
#include <boost/log/trivial.hpp>
#include <boost/log/utility/setup/console.hpp>
#include <boost/program_options.hpp>

#include "AudioStreamer.h"
#include "WebsocketClient.h"
#include "portaudio.h"

namespace po = boost::program_options;
using namespace std;

bool SHUTDOWN = false;

void signal_callback_handler(int signum) { SHUTDOWN = true; }

void run(AudioStreamer& streamer);
void run_mqtt(string mqtt_host, string mqtt_port, string player_name,
              AudioStreamer& streamer);
void mqtt_process_message(mqtt::const_message_ptr msg, AudioStreamer& streamer);

int main(int argc, char* argv[]) {
    // Create signal handler
    signal(SIGINT, signal_callback_handler);

    // Enable Boost logging
    boost::log::add_console_log(std::cout,
                                boost::log::keywords::auto_flush = true);
    int sample_rate;
    string player_name;
    bool save_audio;
    string recordings_directory;
    bool use_mqtt;
    string mqtt_host, mqtt_port;
    string ws_host, ws_port;

    // Process command line options
    try {
        po::options_description desc{"Options"};
        desc.add_options()("help,h", "Help screen")(
            "sample_rate",
            po::value<int>(&sample_rate)->default_value(48000),
            "Sample rate for audio recording")(
            "save_audio",
            po::value<bool>(&save_audio)->default_value(false),
            "Whether or not the audioStreamer will save recorded audio to .wav "
            "file")("recording_directory",
                    po::value<string>(&recordings_directory)
                        ->default_value("recordings"),
                    "The directory to save audio files too")(
            "use_mqtt",
            po::value<bool>(&use_mqtt)->default_value(false),
            "Whether or not the audioStreamer will wait for a Trial "
            "Start message to begin streaming")(
            "mqtt_host",
            po::value<string>(&mqtt_host)->default_value("0.0.0.0"),
            "The host address of the MQTT broker")(
            "mqtt_port",
            po::value<string>(&mqtt_port)->default_value("1883"),
            "The port of the MQTT broker")(
            "ws_host",
            po::value<string>(&ws_host)->default_value("0.0.0.0"),
            "The host address of the websocket server")(
            "ws_port",
            po::value<string>(&ws_port)->default_value("8888"),
            "The port of the websocket server")(
            "player_name",
            po::value<string>(&player_name)->default_value("PLAYER"),
            "The name of the player");

        po::variables_map vm;
        po::store(po::parse_command_line(argc, argv, desc), vm);
        po::notify(vm);
        if (vm.count("help")) {
            cout << desc << "\n";
            return 1;
        }
    }
    catch (const po::error& ex) {
        BOOST_LOG_TRIVIAL(error) << "Error parsing arguments!";
        return -1;
    }

    // Create audio streamer
    AudioStreamer streamer(ws_host,
                           ws_port,
                           player_name,
                           sample_rate,
                           save_audio,
                           recordings_directory);
    if (use_mqtt) {
        BOOST_LOG_TRIVIAL(info) << "Starting audioStreamer in MQTT mode";
        run_mqtt(mqtt_host, mqtt_port, player_name, streamer);
    }
    else {
        BOOST_LOG_TRIVIAL(info) << "Starting audioStreamer in normal mode "
                                   "(CTRL-C will stop streaming)";
        run(streamer);
    }
    BOOST_LOG_TRIVIAL(info)
        << "Close signal recieved, shutting down audio streaming.";
    exit(0);
}

void run(AudioStreamer& streamer) {
    streamer.StartStreaming();
    chrono::milliseconds duration(1);
    while (!SHUTDOWN) {
        // Yield main thread until exit
        this_thread::yield();
        this_thread::sleep_for(duration);
    }
    streamer.StopStreaming();
}

void run_mqtt(string mqtt_host, string mqtt_port, string player_name,
              AudioStreamer& streamer) {
    // Connect to MQTT broker
    string server_address = "tcp://" + mqtt_host + ":" + mqtt_port;
    string client_id = player_name + "_audioStreamer";
    mqtt::async_client mqtt_client(server_address, client_id);

    auto connOpts = mqtt::connect_options_builder()
                        .clean_session(true)
                        .automatic_reconnect(chrono::seconds(2), chrono::seconds(30))
                        .finalize();
  
    try {
	mqtt_client.set_message_callback([&](mqtt::const_message_ptr msg) { mqtt_process_message(msg, streamer); });
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
	streamer.StopStreaming(); // AudioStreamer will check if stream is active when function is called

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

void mqtt_process_message(mqtt::const_message_ptr msg, AudioStreamer& streamer){
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
