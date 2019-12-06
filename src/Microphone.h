#pragma once

#include "portaudio.h"
#include <stdio.h>
#include <stdlib.h>

#include <sndfile.hh>

#define SAMPLE_SILENCE (0.0f)

typedef struct {
  int frame_index; /* Index into sample array. */
  int max_frame_index;
  float* recorded_samples;
} AudioData;

namespace tomcat {

  int recordCallback(const void* input_buffer,
                     void* output_buffer,
                     unsigned long frames_per_buffer,
                     const PaStreamCallbackTimeInfo* time_info,
                     PaStreamCallbackFlags status_flags,
                     void* user_data);

  class PortAudioException;

  /* This routine will be called by the PortAudio engine when audio is needed.
   * It may be called at interrupt level on some machines so don't do anything
   * that could mess up the system like calling malloc() or free().
   */

  class Microphone {
  public:
    Microphone(){};
    void initialize();
    void finalize();
    void set_time_limit_in_seconds(unsigned int);

  private:
    void check_portaudio_error_code();
    void write_data_to_file();
    int total_frames, num_samples, num_bytes;
    AudioData data;
    PaError err = paNoError;
    int output_format = SF_FORMAT_WAV;
    std::string output_filename = "recorded.wav";
    unsigned int num_seconds = 5;
    const unsigned int frames_per_buffer = 512;
    const unsigned int sample_rate = 44100;
  };
} // namespace tomcat
