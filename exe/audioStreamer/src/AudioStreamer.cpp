#include <filesystem>
#include <iostream>
#include <string>
#include <thread>
#include <vector>

#include "portaudio.h"
#include <nlohmann/json.hpp>
#include <sndfile.hh>

#include "AudioStreamer.h"

using namespace std;

AudioStreamer::AudioStreamer(const string& ws_host,
                             const string& ws_port,
                             const string& player_name, int sample_rate,
                             bool save_audio,
                             const string& recordings_directory) {

    this->ws_host = ws_host;
    this->ws_port = ws_port;

    this->player_name = player_name;
    this->sample_rate = sample_rate;

    this->save_audio = save_audio;
    this->recordings_directory = recordings_directory;
    recording_filename = player_name + ".wav";

    ws_client = new WebsocketClient(ws_host, ws_port, player_name, sample_rate);
}

AudioStreamer::~AudioStreamer() { delete ws_client; }

void AudioStreamer::StartStreaming() {
    // Check if already running
    if (running) {
        return;
    }

    running = true;

    // Start audio recording
    if (save_audio) {
        CreateAudioFile();
    }

    // Connect to Websocket server
    ws_client->Connect();

    // Start PortAudio stream
    PaStreamParameters inputParameters;
    PaError err;

    // Initialize PortAudio
    err = Pa_Initialize();
    if (err != paNoError) {
        BOOST_LOG_TRIVIAL(error) << "[ERROR] Failure initializing PortAudio: "
                                 << Pa_GetErrorText(err);
    }

    // Set inputParameters
    inputParameters.device = Pa_GetDefaultInputDevice();
    if (inputParameters.device == paNoDevice) {
        BOOST_LOG_TRIVIAL(error)
            << "[ERROR] Failure getting default audio device: "
            << Pa_GetErrorText(err);
    }
    inputParameters.channelCount = 1;
    inputParameters.sampleFormat = paInt16;
    inputParameters.suggestedLatency =
        Pa_GetDeviceInfo(inputParameters.device)->defaultLowInputLatency;
    inputParameters.hostApiSpecificStreamInfo = nullptr;

    // Open PortAudio stream
    err = Pa_OpenStream(&stream,
                        &inputParameters,
                        nullptr,
                        sample_rate,
                        8196,
                        0,
                        nullptr,
                        nullptr);
    if (err != paNoError) {
        BOOST_LOG_TRIVIAL(error)
            << "[ERROR] Failure initializing PortAudio stream: "
            << Pa_GetErrorText(err);
    }
    err = Pa_StartStream(stream);
    if (err != paNoError) {
        BOOST_LOG_TRIVIAL(error)
            << "[ERROR] Failure starting PortAudio stream: "
            << Pa_GetErrorText(err);
    }

    this->lsl_stream = make_unique<LSLAudioStream>(
        "AudioStreamer", 1, "audio_streamer", "audio_streamer", sample_rate);
    lsl_stream->open();

    BOOST_LOG_TRIVIAL(info) << "[INFO] Started. Recording audio...";
    portaudio_stream_thread = thread([this] { this->Loop(); });
}

void AudioStreamer::StopStreaming() {
    // Check if already running
    if (!running) {
        return;
    }
    running = false;

    // Join loop thread
    portaudio_stream_thread.join();

    PaError err;
    // Stop PortAudio stream
    err = Pa_StopStream(stream);
    if (err != paNoError) {
        BOOST_LOG_TRIVIAL(error)
            << "[ERROR] Failure stopping PortAudio stream: "
            << Pa_GetErrorText(err);
    }

    // shutdown PortAudio
    err = Pa_Terminate();
    if (err != paNoError) {
        BOOST_LOG_TRIVIAL(error) << "[ERROR] Failure shutting down PortAudio: "
                                 << Pa_GetErrorText(err);
    }

    // shutdown websocket client
    ws_client->Shutdown();

    BOOST_LOG_TRIVIAL(info) << "[INFO] Stopped. No longer streaming audio.";
}

void AudioStreamer::Loop() {
    PaError err;
    while ((err = Pa_IsStreamActive(stream)) == 1 && running) {
        vector<int16_t> chunk(8196);
        Pa_ReadStream(stream, (void*)&chunk[0], 8196);
        ws_client->SendChunk(chunk);

        if (save_audio) {
            WriteChunkToFile(chunk);
        }
    }
}

/*
 * Functions to support saving audio as .wav files
 */
void AudioStreamer::ValidateRecordingsDirectory() {
    if (recordings_directory.back() != '/') {
        recordings_directory += "/";
    }
}
void AudioStreamer::GenerateAudioFilename(const nlohmann::json& message) {
    stringstream s;
    string trial_id = message["msg"]["trial_id"];
    string team = message["data"]["group_number"];
    s << "Trial-" << trial_id << "_"
      << "Team-" << team << "_"
      << "Member-" << player_name << ".wav";
    recording_filename = s.str();
}
void AudioStreamer::CreateAudioFile() {
    ValidateRecordingsDirectory();
    string path = recordings_directory + recording_filename;

    this->wave_file = make_unique<WaveWriter>(path, 16, 1, sample_rate);
}
void AudioStreamer::WriteChunkToFile(const vector<int16_t>& chunk) {
    this->wave_file->write_chunk(chunk);
}
