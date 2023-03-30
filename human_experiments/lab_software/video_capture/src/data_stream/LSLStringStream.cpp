#include "LSLStringStream.h"

#include <iostream>

#include "fmt/format.h"

using namespace std;

const int WAIT_FOR_CONSUMER_TIMEOUT = 30; // in seconds

//----------------------------------------------------------------------
// Constructors & Destructor
//----------------------------------------------------------------------
LSLStringStream::LSLStringStream(const std::string& name,
                                 const std::string& source_id,
                                 const std::string& stream_type,
                                 const int sampling_rate) {
    this->stream_info = lsl::stream_info(
        name, stream_type, 1, sampling_rate, lsl::cf_string, source_id);
}

//----------------------------------------------------------------------
// Other functions
//----------------------------------------------------------------------
void LSLStringStream::open() {
    this->outlet =
        make_unique<lsl::stream_outlet>(lsl::stream_outlet(this->stream_info));
    cout << fmt::format(
                "Stream {} is online.Waiting up to {} seconds for consumers",
                this->stream_info.name(),
                WAIT_FOR_CONSUMER_TIMEOUT)
         << endl;
    this->outlet->wait_for_consumers(WAIT_FOR_CONSUMER_TIMEOUT);
    cout << "Consumer detected." << endl;
}

void LSLStringStream::send(const string& message) {
    std::vector<std::string> string_vector;
    string_vector.push_back(message);
    this->outlet->push_sample(string_vector);
}
