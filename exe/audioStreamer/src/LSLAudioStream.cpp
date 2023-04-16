#include "LSLAudioStream.h"

using namespace std;

//----------------------------------------------------------------------
// Constructors & Destructor
//----------------------------------------------------------------------
LSLAudioStream::LSLAudioStream(const std::string& name,
                               int num_channels,
                               const std::string& source_id,
                               const std::string& stream_type,
                               const int sampling_rate)
    : LSLStream(name, num_channels, source_id, stream_type, sampling_rate) {}

//----------------------------------------------------------------------
// Member functions
//----------------------------------------------------------------------
void LSLAudioStream::send(const vector<int16_t>& chunk) {
    size_t chunk_size = chunk.size();
    this->outlet->push_chunk_multiplexed(&chunk[0], chunk_size);
}
