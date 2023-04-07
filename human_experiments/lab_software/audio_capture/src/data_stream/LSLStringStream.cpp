#include "LSLStringStream.h"

using namespace std;

//----------------------------------------------------------------------
// Constructors & Destructor
//----------------------------------------------------------------------
LSLStringStream::LSLStringStream(const std::string& name,
                                 const std::string& source_id,
                                 const std::string& stream_type,
                                 const int sampling_rate)
    : LSLStream(name, 1, source_id, stream_type, sampling_rate) {}

//----------------------------------------------------------------------
// Member functions
//----------------------------------------------------------------------
void LSLStringStream::send(const string& message) {
    std::vector<std::string> string_vector;
    string_vector.push_back(message);
    this->outlet->push_sample(string_vector);
}
