#pragma once

#include <string>
#include <vector>
#include <thread>

#include <portaudio.h>
#include <sndfile.hh>

class Audio {
  public:
    int num_channels;
    int sample_rate;
    PaSampleFormat sample_format;
    int frames_per_buffer;

    Audio(int num_channels,
                  int sample_rate,
                  PaSampleFormat sample_format,
                  int frames_per_buffer);
    ~Audio() = default;

    void start_recording(const std::string& audio_filepath);
    void stop_recording();

  private:
    bool recording = false;

    PaStream* audio_stream;
    std::thread audio_stream_thread;
    SndfileHandle* wav_file;

    void loop_forever();

    void create_audio_file(const std::string& audio_filepath);

    void write_chunk_to_file(const std::vector<int16_t>& chunk);
};
