#pragma once

#include <memory>
#include <string>
#include <thread>
#include <vector>

#include <portaudio.h>

#include "data_stream/WaveWriter.h"
#include "data_stream/LSLAudioStream.h"

class Audio {
  public:
    int num_channels;
    int chunk_size;

    Audio(int num_channels, int chunk_size);
    ~Audio() = default;

    void start_recording(const std::string& out_dir,
                         int sample_rate,
                         std::atomic<bool>* signal_watcher);

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

    /**
     * Creates audio file which audio data will be appended to.
     *
     * @param out_dir: directory where the file must be saved
     * @param sample_rate: audio sample rate
     */
    void create_audio_file(const std::string& out_dir, int sample_rate);

    /**
     * Creates an output directory if it does not exist yet.
     *
     * @param p: path to the directory
     */
    static void create_output_directory(const std::filesystem::path& p);
};
