#include <cstdlib>
#include <signal.h>
#include <unistd.h>

#include <iostream>
#include <string>
#include <thread>

#include <boost/asio/connect.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/websocket.hpp>
#include <boost/log/trivial.hpp>
#include <boost/program_options.hpp>

#include "portaudio.h"

namespace beast = boost::beast;         // from <boost/beast.hpp>
namespace http = beast::http;           // from <boost/beast/http.hpp>
namespace websocket = beast::websocket; // from <boost/beast/websocket.hpp>
namespace net = boost::asio;            // from <boost/asio.hpp>
using tcp = boost::asio::ip::tcp;       // from <boost/asio/ip/tcp.hpp>
using namespace boost::program_options;
using namespace std;

bool RUNNING = true;
void signal_callback_handler(int signum) { RUNNING = false; }

class WebsocketClient {
  public:
    WebsocketClient(string ws_host,
                    string ws_port,
                    string player_name,
                    int sample_rate) {
        this->ws_host = ws_host;
        this->ws_port = ws_port;

        this->player_name = player_name;
        this->sample_rate = sample_rate;
    }
    ~WebsocketClient() {
        if (this->running) {
            this->Shutdown();
        }
    }
    void Connect() {
        // Look up the domain name
        auto const results = resolver.resolve(this->ws_host, this->ws_port);

        // Make the connection on the IP address we get from a lookup
        net::connect(this->ws.next_layer(), results.begin(), results.end());

        // Set a decorator to change the User-Agent of the handshake
        this->ws.set_option(
            websocket::stream_base::decorator([](websocket::request_type& req) {
                req.set(http::field::user_agent,
                        string(BOOST_BEAST_VERSION_STRING) +
                            " websocket-client-coro");
            }));

        // Perform the websocket handshake
        this->ws.handshake(this->ws_host,
                           "/?sampleRate=" + to_string(this->sample_rate) +
                               "&id=" + this->player_name);
        this->ws.binary(true);

        this->running = true;
    }
    void SendChunk(vector<int16_t> chunk) {
        this->ws.write(net::buffer(chunk));
    }
    void Shutdown() {
        this->ws.close(websocket::close_code::normal);
        this->running = false;
    }

  private:
    // Participant data
    string player_name;
    int sample_rate;

    // Websocket options
    string ws_host;
    string ws_port;

    // Connection context
    net::io_context ioc;
    tcp::resolver resolver{ioc};
    websocket::stream<tcp::socket> ws{ioc};
    bool running = false;
};

int main(int argc, char* argv[]) {
    signal(SIGINT, signal_callback_handler);

    int sample_rate;
    string ws_host;
    string ws_port;
    string player_name;
    // Process command line options
    try {
        options_description desc{"Options"};
        desc.add_options()("help,h", "Help screen")(
            "sample_rate",
            value<int>(&sample_rate)->default_value(48000),
            "Sample rate for audio recording")(
            "ws_host",
            value<string>(&ws_host)->default_value("0.0.0.0"),
            "The host address of the websocket server")(
            "ws_port",
            value<string>(&ws_port)->default_value("8888"),
            "The port of the websocket server")(
            "player_name",
            value<string>(&player_name)->default_value("PLAYER"),
            "The name of the player");

        variables_map vm;
        store(parse_command_line(argc, argv, desc), vm);
        notify(vm);
        if (vm.count("help")) {
            cout << desc << "\n";
            return 1;
        }
    }
    catch (const error& ex) {
        BOOST_LOG_TRIVIAL(error) << "Error parsing arguments!";
        return -1;
    }

    // Initialize Websocket Client
    WebsocketClient* client =
        new WebsocketClient(ws_host, ws_port, player_name, sample_rate);
    client->Connect();

    PaStreamParameters inputParameters;
    PaStream* stream;
    PaError err;

    // Initialize PortAudio
    err = Pa_Initialize();
    if (err != paNoError) {
        BOOST_LOG_TRIVIAL(error)
            << "Failure initializing PortAudio: " << Pa_GetErrorText(err);
    }

    // Set inputParameters
    inputParameters.device = Pa_GetDefaultInputDevice();
    if (inputParameters.device == paNoDevice) {
        BOOST_LOG_TRIVIAL(error)
            << "Failure getting default audio device: " << Pa_GetErrorText(err);
    }
    inputParameters.channelCount = 1;
    inputParameters.sampleFormat = paInt16;
    inputParameters.suggestedLatency =
        Pa_GetDeviceInfo(inputParameters.device)->defaultLowInputLatency;
    inputParameters.hostApiSpecificStreamInfo = NULL;

    // Open PortAudio stream
    err = Pa_OpenStream(
        &stream, &inputParameters, NULL, sample_rate, 8196, NULL, NULL, NULL);
    if (err != paNoError) {
        BOOST_LOG_TRIVIAL(error) << "Failure initializing PortAudio stream: "
                                 << Pa_GetErrorText(err);
    }
    err = Pa_StartStream(stream);
    if (err != paNoError) {
        BOOST_LOG_TRIVIAL(error)
            << "Failure starting PortAudio stream: " << Pa_GetErrorText(err);
    }

    while ((err = Pa_IsStreamActive(stream)) == 1 && RUNNING) {
        vector<int16_t> chunk(8196);
        Pa_ReadStream(stream, (void*)&chunk[0], 8196);
        client->SendChunk(chunk);
    }

    // Stop PortAudio stream
    err = Pa_StopStream(stream);
    if (err != paNoError) {
        BOOST_LOG_TRIVIAL(error)
            << "Failure stopping PortAudio stream: " << Pa_GetErrorText(err);
    }

    // Shutdown PortAudio
    err = Pa_Terminate();
    if (err != paNoError) {
        BOOST_LOG_TRIVIAL(error)
            << "Failure shutting down PortAudio: " << Pa_GetErrorText(err);
    }

    // Shutdown websocket client
    client->Shutdown();
    free(client);
}
