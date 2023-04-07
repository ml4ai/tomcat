#include "LSLStream.h"

#include <iostream>

#include "fmt/format.h"

using namespace std;

const int WAIT_FOR_CONSUMER_TIMEOUT = 3; // in seconds

//----------------------------------------------------------------------
// Constructors & Destructor
//----------------------------------------------------------------------
LSLStream::LSLStream(const std::string& name,
                     int num_channels,
                     const std::string& source_id,
                     const std::string& stream_type,
                     const int sampling_rate) {
    this->stream_info = lsl::stream_info(name,
                                         stream_type,
                                         num_channels,
                                         sampling_rate,
                                         lsl::cf_string,
                                         source_id);
}

//----------------------------------------------------------------------
// Other functions
//----------------------------------------------------------------------
void LSLStream::open() {
    this->outlet =
        make_unique<lsl::stream_outlet>(lsl::stream_outlet(this->stream_info));
    cout << fmt::format("[INFO] LSL. Stream Online. {} Stream waiting up to {} "
                        "seconds for consumers",
                        this->stream_info.name(),
                        WAIT_FOR_CONSUMER_TIMEOUT)
         << endl;
    this->outlet->wait_for_consumers(WAIT_FOR_CONSUMER_TIMEOUT);
    if (this->outlet->have_consumers()) {
        cout << "[INFO] LSL. Consumer detected." << endl;
    } else {
        cout << "[WARN] LSL. No consumer detected." << endl;
    }
}
