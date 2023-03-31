#include "Audio.h"

#include <iostream>

#include <fmt/format.h>

#include "common/GeneralException.h"

using namespace std;

Audio::Audio(int num_channels,
             PaSampleFormat sample_format,
             int chunk_size)
    : num_channels(num_channels), sample_format(sample_format),
      chunk_size(chunk_size) {}

void Audio::turn_on() {}

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
        throw GeneralException(fmt::format("Failure initializing PortAudio: {}",
                                           Pa_GetErrorText(err)));
    }

    // Set inputParameters
    input_parameters.device = Pa_GetDefaultInputDevice();
    if (input_parameters.device == paNoDevice) {
        throw GeneralException(fmt::format(
            "Failure getting default audio device: {}", Pa_GetErrorText(err)));
    }
    input_parameters.channelCount = this->num_channels;
    input_parameters.sampleFormat = this->sample_format;
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
        throw GeneralException(fmt::format(
            "Failure initializing PortAudio stream: {}", Pa_GetErrorText(err)));
    }
    err = Pa_StartStream(this->audio_stream);
    if (err != paNoError) {
        throw GeneralException(fmt::format(
            "Failure starting PortAudio stream: {}", Pa_GetErrorText(err)));
    }

    cout << "Recording audio..." << endl;
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

    cout << "Stopping recording..." << endl;
    this->recording = false;

    // Wait for the thread to finish whatever it is doing.
    this->audio_stream_thread.join();

    PaError err;
    // Stop PortAudio stream
    err = Pa_StopStream(this->audio_stream);
    if (err != paNoError) {
        throw GeneralException(fmt::format(
            "Failure stopping PortAudio stream: {}", Pa_GetErrorText(err)));
    }

    // shutdown PortAudio
    err = Pa_Terminate();
    if (err != paNoError) {
        throw GeneralException(fmt::format(
            "Failure shutting down PortAudio: {}", Pa_GetErrorText(err)));
    }

    // The wav file object takes care of cleaning-up on its destructor. So, we
    // don't need to explicitly close it.

    // Close LSL stream
}

void Audio::create_audio_file(const std::string& out_dir, int sample_rate) {
    string audio_filepath = out_dir;
    if (audio_filepath.back() != '/') {
        audio_filepath += "/";
    }
    audio_filepath += "audio.wav";

    int format;
    if (this->sample_format == paInt16) {
        format = SF_FORMAT_WAV | SF_FORMAT_PCM_16;
    }
    else {
        format = SF_FORMAT_WAV | SF_FORMAT_PCM_24;
    }

//    this->wave_file = new SndfileHandle(
//        audio_filepath, SFM_WRITE, format, this->num_channels, sample_rate);

        this->wave_file = make_unique<WaveWriter>(
            audio_filepath, 16, this->num_channels, sample_rate);
}

void Audio::loop() {
    PaError err;
    while ((err = Pa_IsStreamActive(this->audio_stream)) == 1 &&
           this->recording) {
        vector<int16_t> chunk(this->chunk_size);
        Pa_ReadStream(
            this->audio_stream, (void*)&chunk[0], this->chunk_size);

        // Send chunk to LSL

        this->write_chunk_to_file(chunk);
    }
}

void Audio::write_chunk_to_file(const std::vector<int16_t>& chunk) {
//    this->wave_file->write(&chunk[0], static_cast<sf_ count_t>(chunk.size()));
    this->wave_file->write_chunk(chunk);
}