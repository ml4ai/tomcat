#pragma once
#include <string>
#include <thread>
#include <vector>

#include "portaudio.h"
#include <nlohmann/json.hpp>
#include <sndfile.hh>

#include "WebsocketClient.h"
#include "WaveWriter.h"
#include "LSLAudioStream.h"

class AudioStreamer {
  public:
    AudioStreamer(const std::string& ws_host,
                  const std::string& ws_port,
                  const std::string& player_name, int sample_rate,
                  bool save_audio,
                  const std::string& recordings_directory);
    ~AudioStreamer();

    void StartStreaming();
    void StopStreaming();

    void GenerateAudioFilename(const nlohmann::json& message);

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

    // Data for handling wav file writing
    std::unique_ptr<WaveWriter> wave_file;
    std::unique_ptr<LSLAudioStream> lsl_stream;
    bool save_audio;
    std::string recording_filename;
    std::string recordings_directory;
    void ValidateRecordingsDirectory();
    void CreateAudioFile();
    void WriteChunkToFile(const std::vector<int16_t>& chunk);
};
