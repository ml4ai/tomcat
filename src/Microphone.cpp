#include "Microphone.h"

using namespace std;
using namespace fmt::literals;

namespace tomcat {

  const unsigned int num_channels = 2;
  const unsigned int frames_per_buffer = 512;
  const unsigned int num_seconds = 5;
  const unsigned int sample_rate = 44100;

  class PortAudioException : public runtime_error {
  public:
    PortAudioException(PaError err)
        : runtime_error(
              "An error occured while using the portaudio stream.\n"
              "Error : {0} ({1})\n"_format(err, Pa_GetErrorText(err))) {}

    PortAudioException(string msg) : runtime_error(msg) {}
  };

  /* This routine will be called by the PortAudio engine when audio is needed.
   * It may be called at interrupt level on some machines so don't do anything
   * that could mess up the system like calling malloc() or free().
   */

  int recordCallback(const void* input_buffer,
                     void* output_buffer,
                     unsigned long frames_per_buffer,
                     const PaStreamCallbackTimeInfo* time_info,
                     PaStreamCallbackFlags status_flags,
                     void* user_data) {

    /* Prevent unused variable warnings. */
    (void)output_buffer;
    (void)time_info;
    (void)status_flags;
    (void)user_data;

    AudioData* data = (AudioData*)user_data;
    unsigned long frames_left = data->max_frame_index - data->frame_index;

    long frames_to_calc;
    int callback_return_code;

    if (frames_left < frames_per_buffer) {
      frames_to_calc = frames_left;
      callback_return_code = paComplete;
    }
    else {
      frames_to_calc = frames_per_buffer;
      callback_return_code = paContinue;
    }

    float* wptr = &data->recorded_samples[data->frame_index * num_channels];
    const float* rptr = (const float*)input_buffer;
    if (input_buffer == NULL) {
      for (int i = 0; i < frames_to_calc; i++) {
        *wptr++ = SAMPLE_SILENCE; /* left */
        if (num_channels == 2) {
          *wptr++ = SAMPLE_SILENCE; /* right */
        }
      }
    }
    else {
      for (int i = 0; i < frames_to_calc; i++) {
        *wptr++ = *rptr++; /* left */
        if (num_channels == 2) {
          *wptr++ = *rptr++; /* right */
        }
      }
    }
    data->frame_index += frames_to_calc;
    return callback_return_code;
  }

  void Microphone::initialize() {
    PaStreamParameters input_parameters, outputParameters;
    PaStream* stream;

    /* Record for NUM_SECONDS seconds. */
    this->data.max_frame_index = this->total_frames = NUM_SECONDS * sample_rate;
    this->data.frame_index = 0;
    this->num_samples = this->total_frames * num_channels;
    this->num_bytes = this->num_samples * sizeof(float);

    /* From now on, recorded_samples is initialised. */
    this->data.recorded_samples = (float*)malloc(this->num_bytes);
    if (this->data.recorded_samples == NULL) {
      throw PortAudioException("Could not allocate record array.\n");
    }

    for (int i = 0; i < this->num_samples; i++) {
      this->data.recorded_samples[i] = 0;
    }

    this->err = Pa_Initialize();
    this->check_portaudio_error_code();

    input_parameters.device = Pa_GetDefaultInputDevice();
    if (input_parameters.device == paNoDevice) {
      throw PortAudioException("Error: No default input device.\n");
    }
    input_parameters.channelCount = 2; /* stereo input */
    input_parameters.sampleFormat = paInt16;
    input_parameters.suggestedLatency =
        Pa_GetDeviceInfo(input_parameters.device)->defaultLowInputLatency;
    input_parameters.hostApiSpecificStreamInfo = NULL;

    /* Record some audio. -------------------------------------------- */
    this->err = Pa_OpenStream(&stream,
                              &input_parameters,
                              NULL, /* &outputParameters, */
                              sample_rate,
                              frames_per_buffer,
                              paClipOff, /* we won't output out of range samples
                                            so don't bother clipping them */
                              recordCallback,
                              &this->data);
    this->check_portaudio_error_code();

    this->err = Pa_StartStream(stream);
    this->check_portaudio_error_code();

    while ((this->err = Pa_IsStreamActive(stream)) == 1) {
      Pa_Sleep(1000);
    }
    this->check_portaudio_error_code();

    this->err = Pa_CloseStream(stream);
    this->check_portaudio_error_code();
  };

  void Microphone::write_data_to_file() {

    string fname = "recorded.wav";
    int channels = 2;

    int BUFFER_LEN = 1024;
    float buffer[BUFFER_LEN];
    auto file =
        SndfileHandle(fname, SFM_WRITE, SF_FORMAT_WAV, channels, sample_rate);

    file.writef(this->data.recorded_samples, this->total_frames);

    // FILE* fid;
    // fid = fopen("recorded.raw", "wb");
    // if (fid == NULL) {
    // printf("Could not open file.");
    //}
    // else {
    // fwrite(this->data.recorded_samples,
    // num_channels * sizeof(float),
    // this->total_frames,
    // fid);
    // fclose(fid);
    // printf("Wrote data to 'recorded.raw'\n");
    //}
  }

  void Microphone::finalize() {
    this->write_data_to_file();
    Pa_Terminate();
    if (this->data.recorded_samples) { /* Sure it is NULL or valid. */
      free(this->data.recorded_samples);
    }
  };

  void Microphone::check_portaudio_error_code() {
    if (this->err != paNoError) {
      throw PortAudioException(this->err);
    }
  };
} // namespace tomcat
