#include <boost/program_options.hpp>

#include <atomic>
#include <iostream>

#include "common/SignalHandler.h"
#include "media/Audio.h"

// Mono is better for audio analysis. Stereo is better for music listeners.
#define DEFAULT_CHANNEL_COUNT 1
#define DEFAULT_SAMPLE_RATE 48000
#define DEFAULT_CHUNK_SIZE 8192

using namespace std;
namespace po = boost::program_options;

int main(int argc, const char* argv[]) {
    string out_dir;
    string device_name;
    int sample_rate;
    int num_channels;
    int chunk_size;
    int device_index;

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
        "chunk_size",
        po::value<int>(&chunk_size)
            ->default_value(DEFAULT_CHUNK_SIZE)
            ->required(),
        "Size of the buffer.")(
        "device_index",
        po::value<int>(&device_index)->default_value(-1),
        "Index of the input device. -1 for default.")("device_name",
                                po::value<string>(&device_name),
                                "Name of the input device. If a name if passed, it "
                                "has priority over the device index.");

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

    unique_ptr<Audio> audio;
    if (device_name.empty()) {
        audio = make_unique<Audio>(num_channels, chunk_size, device_index);
    } else{
        audio = make_unique<Audio>(num_channels, chunk_size, device_name);
    }

    // Signal handler in case the program is interrupted.
    watch_for_signal();

    try {
        audio->start_recording(out_dir, sample_rate, &quit);
    }
    catch (const std::exception& ex) {
        cerr << "[ERROR] Program crashed." << endl;
        cerr << ex.what() << endl;
    }

    return 0;
}
