#include "LSLStringStream.h"

#include "fmt/format.h"

using namespace std;

//----------------------------------------------------------------------
// Constructors & Destructor
//----------------------------------------------------------------------
LSLStringStream::LSLStringStream(const std::string& name,
                                 const std::string& source_id,
                                 const std::string& stream_type) {
    lsl::stream_info info(
        name, stream_type, 1, lsl::IRREGULAR_RATE, lsl::cf_string, source_id);
    this->outlet = make_unique<lsl::stream_outlet>(lsl::stream_outlet(info));
}

//----------------------------------------------------------------------
// Other functions
//----------------------------------------------------------------------
void LSLStringStream::send(const string& message) {
    std::vector<std::string> string_vector;
    string_vector.push_back(message);
    this->outlet->push_sample(string_vector);
}
