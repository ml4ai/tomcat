#pragma once

#include <string>
#include <vector>

#include "LSLStream.h"

class LSLAudioStream : public LSLStream {
  public:
    LSLAudioStream(const std::string& name,
                   int num_channels,
                   const std::string& source_id,
                   const std::string& stream_type,
                   int sampling_rate);
    ~LSLAudioStream() = default;

    LSLAudioStream(const LSLAudioStream&) = delete;
    LSLAudioStream& operator=(const LSLAudioStream&) = delete;
    LSLAudioStream(LSLAudioStream&&) = default;
    LSLAudioStream& operator=(LSLAudioStream&&) = default;

    /**
     * Pushes a message to LSL.
     *
     * @param message: audio data chunk
     */
    void send(const std::vector<int16_t>& chunk);
};
