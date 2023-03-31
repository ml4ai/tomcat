#include <boost/program_options.hpp>

#include <atomic>
#include <iostream>

#include "common/SignalHandler.h"
#include "media/Audio.h"

// Mono is better for audio analysis. Stereo is better for music listeners.
#define DEFAULT_CHANNEL_COUNT 1
#define DEFAULT_SAMPLE_RATE 48000
// We keep it at 16 for compatibility with Google Speech-to-Text API
#define DEFAULT_SAMPLE_FORMAT 16
#define DEFAULT_CHUNK_SIZE 4096
//    8196

using namespace std;
namespace po = boost::program_options;

int main(int argc, const char* argv[]) {
    string out_dir;
    int sample_rate;
    int num_channels;
    int sample_format;
    int chunk_size;

    po::options_description arguments("Program Options");
    arguments.add_options()("help,h",
                            "This program records audio to a file and pushes "
                            "chunks to an LSL stream.")(
        "out_dir",
        po::value<string>(&out_dir)->required(),
        "Directory where the audio file must be saved.")(
        "sample_rate",
        po::value<int>(&sample_rate)
            ->default_value(DEFAULT_SAMPLE_RATE)
            ->required(),
        "Sample rate.")("num_channels",
                        po::value<int>(&num_channels)
                            ->default_value(DEFAULT_CHANNEL_COUNT)
                            ->required(),
                        "Number of channels. 1 for mono, 2 for stereo.")(
        "sample_format",
        po::value<int>(&sample_format)
            ->default_value(DEFAULT_SAMPLE_FORMAT)
            ->required(),
        "16 or 24.")(
        "chunk_size",
        po::value<int>(&chunk_size)
            ->default_value(DEFAULT_CHUNK_SIZE)
            ->required(),
        "Size of the buffer.");

    po::variables_map vm;
    po::store(po::parse_command_line(argc, argv, arguments), vm);
    po::notify(vm);
    if (vm.count("help")) {
        cout << arguments << "\n";
        return 1;
    }

    if (num_channels != 1 and num_channels != 2) {
        cerr << "The number of channels must be either 1 (mono) or 2 (stereo)."
             << endl;
    }

    PaSampleFormat pa_sample_format;
    if (sample_format == 16) {
        pa_sample_format = paInt16;
    }
    else if (sample_format == 24) {
        pa_sample_format = paInt24;
    }
    else {
        cerr << "Sample format not accepted. The values accepted are 16 or 24."
             << endl;
        return 1;
    }

    Audio audio(num_channels, pa_sample_format, chunk_size);

    // Signal handler in case the program is interrupted.
    watch_for_signal();

    audio.turn_on();
    audio.start_recording(out_dir, sample_rate, &quit);

    return 0;
}
