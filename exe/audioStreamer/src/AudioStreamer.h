#pragma once
#include <string>
#include <thread>

#include "portaudio.h"

#include "WebsocketClient.h"

class AudioStreamer {
  public:
    AudioStreamer(std::string ws_host, std::string ws_port,
                  std::string player_name, int sample_rate);
    ~AudioStreamer();

    void StartStreaming();
    void StopStreaming();

  private:
    bool running = false;
    int sample_rate;
    std::string player_name;

    // Websocket  connection
    std::string ws_host, ws_port;
    WebsocketClient* ws_client;

    // Portaudio stream
    std::thread portaudio_stream_thread;
    PaStream* stream;
    void Loop();
};
