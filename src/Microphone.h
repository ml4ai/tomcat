#pragma once

#include "portaudio.h"
#include <stdio.h>
#include <stdlib.h>

#include <fmt/format.h>
#include <sndfile.hh>

#define SAMPLE_SILENCE (0.0f)
const unsigned int NUM_SECONDS = 5;

typedef struct {
  int frame_index; /* Index into sample array. */
  int max_frame_index;
  float* recorded_samples;
} AudioData;

namespace tomcat {

  class PortAudioException;

  /* This routine will be called by the PortAudio engine when audio is needed.
   * It may be called at interrupt level on some machines so don't do anything
   * that could mess up the system like calling malloc() or free().
   */

  int recordCallback(const void* input_buffer,
                     void* output_buffer,
                     unsigned long frames_per_buffer,
                     const PaStreamCallbackTimeInfo* time_info,
                     PaStreamCallbackFlags status_flags,
                     void* user_data);

  class Microphone {
  public:
    Microphone(){};

    void initialize();
    void write_data_to_file();
    void finalize();
    void check_portaudio_error_code();

  private:
    int total_frames, num_samples, num_bytes;
    AudioData data;
    PaError err = paNoError;
  };

} // namespace tomcat
