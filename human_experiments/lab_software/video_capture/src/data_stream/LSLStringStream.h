#pragma once

#include <string>

#include "LSLStream.h"

class LSLStringStream : public LSLStream {
  public:
    LSLStringStream(const std::string& name,
                    const std::string& source_id,
                    const std::string& stream_type,
                    int sampling_rate);
    ~LSLStringStream() = default;

    LSLStringStream(const LSLStringStream&) = delete;
    LSLStringStream& operator=(const LSLStringStream&) = delete;
    LSLStringStream(LSLStringStream&&) = default;
    LSLStringStream& operator=(LSLStringStream&&) = default;

    /**
     * Pushes a message to LSL.
     *
     * @param message: string message
     */
    void send(const std::string& message);
};
