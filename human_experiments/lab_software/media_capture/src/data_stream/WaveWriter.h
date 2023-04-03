#pragma on

#include <fstream>
#include <vector>

class WaveWriter {
  public:
    WaveWriter(const std::string& audio_filepath,
               int audio_format,
               int num_channels,
               int sample_rate);
    ~WaveWriter();

    /**
     * Writes a chunk of data to the file.
     *
     * @param chunk: data chunk
     */
    void write_chunk(const std::vector<int16_t>& chunk);

  private:
    std::ofstream wave_file;

    /**
     * Helper function to write an integer as bytes.
     *
     * @param value: value to write
     * @param byte_size: number of bytes to write
     */
    void write_as_bytes(int value, int byte_size);

    /**
     * Updates the file header with the size related fields in the audio file
     */
    void update_header();
};
