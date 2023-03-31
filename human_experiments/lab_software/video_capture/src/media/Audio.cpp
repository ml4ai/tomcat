#include "Audio.h"

#include <fmt/format.h>

#include "common/GeneralException.h"

using namespace std;

Audio::Audio(int num_channels,
                             int sample_rate,
                             PaSampleFormat sample_format,
                             int frames_per_buffer)
    : num_channels(num_channels), sample_rate(sample_rate),
      sample_format(sample_format), frames_per_buffer(frames_per_buffer) {}

void Audio::start_recording(const string& audio_filepath) {
    if (this->recording) {
        // Ignore if it's already recording.
        return;
    }

    recording = true;

    this->create_audio_file(audio_filepath);

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
                        this->sample_rate,
                        this->frames_per_buffer,
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

    this->audio_stream_thread = thread([this] { this->loop_forever(); });
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
        throw GeneralException(fmt::format(
            "Failure stopping PortAudio stream: {}", Pa_GetErrorText(err)));
    }

    // shutdown PortAudio
    err = Pa_Terminate();
    if (err != paNoError) {
        throw GeneralException(fmt::format(
            "Failure shutting down PortAudio: {}", Pa_GetErrorText(err)));
    }

    // Close LSL stream
}

void Audio::create_audio_file(const std::string& audio_filepath) {
    this->wav_file = new SndfileHandle(audio_filepath,
                                       SFM_WRITE,
                                       SF_FORMAT_WAV | SF_FORMAT_PCM_16,
                                       this->num_channels,
                                       this->sample_rate);
}

void Audio::loop_forever() {
    PaError err;
    while ((err = Pa_IsStreamActive(this->audio_stream)) == 1 &&
           this->recording) {
        vector<int16_t> chunk(this->frames_per_buffer);
        Pa_ReadStream(
            this->audio_stream, (void*)&chunk[0], this->frames_per_buffer);

        // Send chunk to LSL

        this->write_chunk_to_file(chunk);
    }
}

void Audio::write_chunk_to_file(const std::vector<int16_t>& chunk) {
    this->wav_file->write(&chunk[0], chunk.size());
}