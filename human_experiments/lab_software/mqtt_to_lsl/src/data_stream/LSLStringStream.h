#pragma once

#include <string>

#include <lsl_cpp.h>
#include <mosquitto.h>

class LSLStringStream {
  public:
    LSLStringStream(const std::string& name,
                    const std::string& source_id,
                    const std::string& stream_type);
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

  private:
    std::unique_ptr<lsl::stream_outlet> outlet;
};
