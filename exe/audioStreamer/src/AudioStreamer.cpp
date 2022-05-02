#include <string>
#include <thread>
#include <vector>

#include "portaudio.h"

#include "AudioStreamer.h"
#include "WebsocketClient.h"

using namespace std;

AudioStreamer::AudioStreamer(std::string ws_host, std::string ws_port,
                             std::string player_name, int sample_rate) {

    this->ws_host = ws_host;
    this->ws_port = ws_port;

    this->player_name = player_name;
    this->sample_rate = sample_rate;

    ws_client = new WebsocketClient(ws_host, ws_port, player_name, sample_rate);
}

AudioStreamer::~AudioStreamer() { delete ws_client; }

void AudioStreamer::StartStreaming() {
    running = true;

    // Connect to Websocket server
    ws_client->Connect();

    // Start PortAudio stream
    PaStreamParameters inputParameters;
    PaError err;

    // Initialize PortAudio
    err = Pa_Initialize();
    if (err != paNoError) {
        BOOST_LOG_TRIVIAL(error)
            << "Failure initializing PortAudio: " << Pa_GetErrorText(err);
    }

    // Set inputParameters
    inputParameters.device = Pa_GetDefaultInputDevice();
    if (inputParameters.device == paNoDevice) {
        BOOST_LOG_TRIVIAL(error)
            << "Failure getting default audio device: " << Pa_GetErrorText(err);
    }
    inputParameters.channelCount = 1;
    inputParameters.sampleFormat = paInt16;
    inputParameters.suggestedLatency =
        Pa_GetDeviceInfo(inputParameters.device)->defaultLowInputLatency;
    inputParameters.hostApiSpecificStreamInfo = NULL;

    // Open PortAudio stream
    err = Pa_OpenStream(
        &stream, &inputParameters, NULL, sample_rate, 8196, NULL, NULL, NULL);
    if (err != paNoError) {
        BOOST_LOG_TRIVIAL(error) << "Failure initializing PortAudio stream: "
                                 << Pa_GetErrorText(err);
    }
    err = Pa_StartStream(stream);
    if (err != paNoError) {
        BOOST_LOG_TRIVIAL(error)
            << "Failure starting PortAudio stream: " << Pa_GetErrorText(err);
    }

    portaudio_stream_thread = thread([this] { this->Loop(); });
}

void AudioStreamer::StopStreaming() {
    running = false;
    
    // Join loop thread
    portaudio_stream_thread.join();

    PaError err;
    // Stop PortAudio stream
    err = Pa_StopStream(stream);
    if (err != paNoError) {
        BOOST_LOG_TRIVIAL(error)
            << "Failure stopping PortAudio stream: " << Pa_GetErrorText(err);
    }

    // shutdown PortAudio
    err = Pa_Terminate();
    if (err != paNoError) {
        BOOST_LOG_TRIVIAL(error)
            << "Failure shutting down PortAudio: " << Pa_GetErrorText(err);
    }

    // shutdown websocket client
    ws_client->Shutdown();
    
}

void AudioStreamer::Loop() {
    PaError err;
    while ((err = Pa_IsStreamActive(stream)) == 1 && running) {
        vector<int16_t> chunk(8196);
        Pa_ReadStream(stream, (void*)&chunk[0], 8196);
        ws_client->SendChunk(chunk);
    }
}
