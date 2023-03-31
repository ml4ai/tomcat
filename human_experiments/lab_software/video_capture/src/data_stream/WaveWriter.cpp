#include "WaveWriter.h"

#include <iostream>
#include <vector>

#include <fmt/format.h>

#include "common/GeneralException.h"

using namespace std;

WaveWriter::WaveWriter(const std::string& audio_filepath,
                       int bits_per_sample,
                       int num_channels,
                       int sample_rate) {

    this->wave_file.open(audio_filepath, ios::binary);
    if (!this->wave_file.is_open()) {
        throw GeneralException(
            fmt::format("Could not create the file {}.", audio_filepath));
    }

    // WAVE PCM soundfile format: http://soundfile.sapp.org/doc/WaveFormat/

    // RIFF chunk
    this->wave_file << "RIFF";
    this->wave_file << "____"; // chunk size (only known in the end)
    this->wave_file << "WAVE";

    // FMT sub-chunk
    int byte_rate = sample_rate * num_channels * (bits_per_sample / 8);
    int block_align = num_channels * (bits_per_sample / 8);

    this->wave_file << "fmt ";                // sub-chunk1 ID
    this->write_as_bytes(bits_per_sample, 4); // sub-chunk1 size
    this->write_as_bytes(1, 2);               // audio format = PCM
    this->write_as_bytes(num_channels, 2);    // #channels
    this->write_as_bytes(sample_rate, 4);     // sample rate
    this->write_as_bytes(byte_rate, 4);       // byte rate
    this->write_as_bytes(block_align, 2);     // block align
    this->write_as_bytes(bits_per_sample, 2); // bits per sample

    // Data sub-chunk
    this->wave_file << "data"; // sub-chunk2 ID
    this->wave_file << "----"; // sub-chunk2 size (only known in the end)

    this->start_audio_pos = this->wave_file.tellp();
}

WaveWriter::~WaveWriter() {
    // Write the size related fields in the audio file

    // Move 4 bytes above the start of the audio to fill the sub-chunk2 size.
    long long end_audio_pos = this->wave_file.tellp();
    this->wave_file.seekp(this->start_audio_pos - 4);
    int data_size = end_audio_pos - start_audio_pos;

    this->write_as_bytes(data_size, 4);

    // Move 4 bytes after the beginning of the file.
    this->wave_file.seekp(ios::beg + 4);
    this->write_as_bytes(end_audio_pos - 8, 4);

    this->wave_file.close();
}

void WaveWriter::write_chunk(const std::vector<short>& chunk) {
    int chunk_size = chunk.size();
    this->wave_file.write(reinterpret_cast<const char*>(&chunk[0]),
                          chunk_size * sizeof(chunk[0]));
}

void WaveWriter::write_as_bytes(int value, int byte_size) {
    this->wave_file.write(reinterpret_cast<const char*>(&value), byte_size);
}