#pragma once

#include <memory>
#include <string>
#include <thread>
#include <vector>

#include <portaudio.h>

#include "Device.h"
#include "data_stream/WaveWriter.h"
#include "data_stream/LSLAudioStream.h"

class Audio : public Device {
  public:
    int num_channels;
    int chunk_size;

    Audio(int num_channels, int chunk_size);
    ~Audio() override = default;

    void start_recording(const std::string& out_dir,
                         int sample_rate,
                         std::atomic<bool>* signal_watcher) override;

    void stop_recording();

  private:
    bool recording = false;

    PaStream* audio_stream;
    std::thread audio_stream_thread;
    std::unique_ptr<WaveWriter> wave_file;
    std::unique_ptr<LSLAudioStream> lsl_stream;

    /**
     * Loop until interruption.
     */
    void loop();

    void create_audio_file(const std::string& out_dir, int sample_rate);
};
