#pragma on

#include <fstream>

class WaveWriter {
  public:
    WaveWriter(const std::string& audio_filepath,
               int audio_format,
               int num_channels,
               int sample_rate);
    ~WaveWriter();

    void write_chunk(const std::vector<short>& chunk);

  private:
    std::ofstream wave_file;

    // So we can compute the size of the audio in the end.
    long long start_audio_pos;

    void write_as_bytes(int value, int byte_size);
};
