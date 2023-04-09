#include "Audio.h"

#include <iostream>

#include <fmt/format.h>

#include "common/GeneralException.h"

// For compatibility with other modules
#define SAMPLE_FORMAT paInt16
#define BITS_PER_SAMPLE 16

using namespace std;
namespace fs = boost::filesystem;

Audio::Audio(int num_channels, int chunk_size)
    : num_channels(num_channels), chunk_size(chunk_size),
      audio_stream(nullptr) {}

void Audio::start_recording(const string& out_dir,
                            int sample_rate,
                            atomic<bool>* signal_watcher) {
    if (this->recording) {
        // Ignore if it's already recording.
        return;
    }

    recording = true;

    const filesystem::path p(out_dir);
    create_output_directory(p);
    this->create_audio_file(out_dir, sample_rate);

    // Start portaudio stream
    PaStreamParameters input_parameters;
    PaError err;

    // Initialize PortAudio
    err = Pa_Initialize();
    if (err != paNoError) {
        throw GeneralException(
            fmt::format("[ERROR] Failure initializing PortAudio: {}",
                        Pa_GetErrorText(err)));
    }

    // Set inputParameters
    input_parameters.device = Pa_GetDefaultInputDevice();
    if (input_parameters.device == paNoDevice) {
        throw GeneralException(
            fmt::format("[ERROR] Failure getting default audio device: {}",
                        Pa_GetErrorText(err)));
    }
    input_parameters.channelCount = this->num_channels;
    input_parameters.sampleFormat = SAMPLE_FORMAT;
    input_parameters.suggestedLatency =
        Pa_GetDeviceInfo(input_parameters.device)->defaultLowInputLatency;
    input_parameters.hostApiSpecificStreamInfo = nullptr;

    // Open PortAudio stream
    err = Pa_OpenStream(&this->audio_stream,
                        &input_parameters,
                        nullptr,
                        sample_rate,
                        this->chunk_size,
                        0,
                        nullptr,
                        nullptr);
    if (err != paNoError) {
        throw GeneralException(
            fmt::format("[ERROR] Failure initializing PortAudio stream: {}",
                        Pa_GetErrorText(err)));
    }
    err = Pa_StartStream(this->audio_stream);
    if (err != paNoError) {
        throw GeneralException(
            fmt::format("[ERROR] Failure starting PortAudio stream: {}",
                        Pa_GetErrorText(err)));
    }

    cout << fmt::format("[INFO] Default audio device is {}.",
                        Pa_GetDeviceInfo(input_parameters.device)->name)
         << endl;

    this->lsl_stream = make_unique<LSLAudioStream>(
        "Audio", this->num_channels, "audio", "audio", sample_rate);
    lsl_stream->stream_info.desc()
        .append_child("provider")
        .append_child_value("api", "portaudio19");
    lsl_stream->stream_info.desc().append_child_value(
        "device", Pa_GetDeviceInfo(input_parameters.device)->name);
    lsl_stream->stream_info.desc().append_child_value(
        "channel_format", "int16");
    lsl_stream->open();

    cout << "[INFO] Started. Recording audio..." << endl;
    this->audio_stream_thread = thread([this] { this->loop(); });

    while (!signal_watcher->load()) {
        // Do nothing. The audio recording thread will be active and running
        // at this moment. When the variable signal_watcher changes to true by
        // a program interruption, we leave this loop and stop recording.
    }

    this->stop_recording();
}

void Audio::stop_recording() {
    // Ignore if it's not recording
    if (!this->recording) {
        return;
    }

    this->recording = false;

    // Wait for the thread to finish whatever it is doing.
    this->audio_stream_thread.join();

    PaError err;
    // Stop PortAudio stream
    err = Pa_StopStream(this->audio_stream);
    if (err != paNoError) {
        throw GeneralException(
            fmt::format("[ERROR] Failure stopping PortAudio stream: {}",
                        Pa_GetErrorText(err)));
    }

    // shutdown PortAudio
    err = Pa_Terminate();
    if (err != paNoError) {
        throw GeneralException(
            fmt::format("[ERROR] Failure shutting down PortAudio: {}",
                        Pa_GetErrorText(err)));
    }

    // The wav file object takes care of cleaning-up on its destructor. So, we
    // don't need to explicitly close it.

    cout << "[INFO] Stopped. No longer recording audio." << endl;
}

void Audio::create_audio_file(const std::string& out_dir, int sample_rate) {
    string audio_filepath = out_dir;
    if (audio_filepath.back() != '/') {
        audio_filepath += "/";
    }
    audio_filepath += "audio.wav";

    this->wave_file = make_unique<WaveWriter>(
        audio_filepath, BITS_PER_SAMPLE, this->num_channels, sample_rate);
}

void Audio::loop() {
    PaError err;
    while ((err = Pa_IsStreamActive(this->audio_stream)) == 1 &&
           this->recording) {
        vector<int16_t> chunk(this->chunk_size);
        Pa_ReadStream(this->audio_stream, (void*)&chunk[0], this->chunk_size);

        this->lsl_stream->send(chunk);
        this->wave_file->write_chunk(chunk);
    }
}

void Audio::create_output_directory(const filesystem::path& p) {
    if (std::filesystem::exists(p)) {
        cout << fmt::format(
                    "[INFO] Writing frames to already existing path: {}",
                    p.string())
             << endl;
    }
    else {
        cout << fmt::format(
                    "[INFO] Directory: {} does not exist. Creating it now.",
                    p.string())
             << endl;
        std::filesystem::create_directories(p);
    }
}