#pragma once

#include <string>
#include <thread>
#include <vector>
#include <memory>

#include <portaudio.h>
#include <sndfile.hh>

#include "Device.h"
#include "data_stream/WaveWriter.h"

class Audio : public Device {
  public:
    int num_channels;
    PaSampleFormat sample_format;
    int chunk_size;

    Audio(int num_channels,
          PaSampleFormat sample_format,
          int chunk_size);
    ~Audio() = default;

    /**
     * Does initial setup for audio recording.
     */
    void turn_on() override;

    void start_recording(const std::string& out_dir,
                         int sample_rate,
                         std::atomic<bool>* signal_watcher) override;

    void stop_recording();

  private:
    bool recording = false;

    PaStream* audio_stream;
    std::thread audio_stream_thread;
    std::unique_ptr<WaveWriter> wave_file;
//    SndfileHandle* wave_file;

    void loop();

    void create_audio_file(const std::string& out_dir, int sample_rate);

    void write_chunk_to_file(const std::vector<int16_t>& chunk);
};
