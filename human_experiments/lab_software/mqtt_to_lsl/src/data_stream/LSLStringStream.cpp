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
                                 const std::string& stream_type) {
    lsl::stream_info info(
        name, stream_type, 1, lsl::IRREGULAR_RATE, lsl::cf_string, source_id);
    this->outlet = make_unique<lsl::stream_outlet>(lsl::stream_outlet(info));
    cout << fmt::format(
                "Stream {} is online.Waiting up to {} seconds for consumers",
                name,
                WAIT_FOR_CONSUMER_TIMEOUT)
         << endl;
    this->outlet->wait_for_consumers(WAIT_FOR_CONSUMER_TIMEOUT);
    cout << "Consumer detected." << endl;
}

//----------------------------------------------------------------------
// Other functions
//----------------------------------------------------------------------
void LSLStringStream::open() {
    std::vector<std::string> string_vector;
    string_vector.push_back(message);
    this->outlet->push_sample(string_vector);
}

void LSLStringStream::send(const string& message) {
    std::vector<std::string> string_vector;
    string_vector.push_back(message);
    this->outlet->push_sample(string_vector);
}
