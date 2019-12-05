#include "Microphone.h"
#include <fmt/format.h>

using namespace std;
using namespace fmt::literals;

namespace tomcat {

  const unsigned int num_channels = 2;

  class PortAudioException : public runtime_error {
  public:
    PortAudioException(PaError err)
        : runtime_error(
              "An error occured while using the portaudio stream.\n"
              "Error : {0} ({1})\n"_format(err, Pa_GetErrorText(err))) {}

    PortAudioException(string msg) : runtime_error(msg) {}
  };

  void Microphone::set_time_limit_in_seconds(unsigned int num_seconds) {
    this->num_seconds = num_seconds;
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

    float* wptr =
        &data->recorded_samples[data->frame_index * num_channels];
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
    PaStream* stream;

    /* Record for num_seconds seconds. */
    this->data.max_frame_index = this->total_frames =
        this->num_seconds * this->sample_rate;
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

    int device = Pa_GetDefaultInputDevice();
    if (device == paNoDevice) {
      throw PortAudioException("Error: No default input device.\n");
    }

    this->err = Pa_OpenDefaultStream(&stream,
                              Pa_GetDeviceInfo(device)->maxInputChannels,
                              Pa_GetDeviceInfo(device)->maxOutputChannels,
                              paFloat32,
                              this->sample_rate,
                              this->frames_per_buffer,
                              recordCallback,
                              &this->data);

    this->check_portaudio_error_code();

    this->err = Pa_StartStream(stream);
    this->check_portaudio_error_code();

    while ((err = Pa_IsStreamActive(stream)) == 1) {
      Pa_Sleep(10);
    }

    this->err = Pa_CloseStream(stream);
    this->check_portaudio_error_code();
  };

  void Microphone::write_data_to_file() {
    auto file = SndfileHandle(this->output_filename,
                              SFM_WRITE,
                              SF_FORMAT_WAV | SF_FORMAT_FLOAT,
                              num_channels,
                              this->sample_rate);

    file.write(this->data.recorded_samples, this->num_samples);
  }

  void Microphone::finalize() {
    this->write_data_to_file();
    Pa_Terminate();
    if (this->data.recorded_samples) {
      free(this->data.recorded_samples);
    }
  };

  void Microphone::check_portaudio_error_code() {
    if (this->err != paNoError) {
      throw PortAudioException(this->err);
    }
  };
} // namespace tomcat
