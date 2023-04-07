#pragma once

#include <string>

#include <lsl_cpp.h>

class LSLStream {
  public:
    lsl::stream_info stream_info;

    LSLStream(const std::string& name,
              int num_channels,
              const std::string& source_id,
              const std::string& stream_type,
              int sampling_rate);
    ~LSLStream() = default;

    LSLStream(const LSLStream&) = delete;
    LSLStream& operator=(const LSLStream&) = delete;
    LSLStream(LSLStream&&) = default;
    LSLStream& operator=(LSLStream&&) = default;

    /**
     * Opens the stream and wait for consumers.
     *
     */
    void open();

  protected:
    std::unique_ptr<lsl::stream_outlet> outlet;
};
