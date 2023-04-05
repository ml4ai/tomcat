#include <boost/program_options.hpp>

#include <atomic>
#include <exception>
#include <iostream>

#include <boost/algorithm/string.hpp>

#include "common/SignalHandler.h"
#include "media/Device.h"
#include "media/Screen.h"
#include "media/Webcam.h"

using namespace std;
namespace po = boost::program_options;

int main(int argc, const char* argv[]) {
    string device_name;
    string out_dir;
    string camera_name;
    int camera_index;
    int fps;
    int width;
    int height;

    po::options_description arguments("Program Options");
    arguments.add_options()(
        "help,h",
        "This program records images from either a webcam or the screen.")(
        "out_dir",
        po::value<string>(&out_dir)->required(),
        "Directory where the images must be saved.")(
        "camera_index",
        po::value<int>(&camera_index)->default_value(0),
        "Index of the camera.")("camera_name",
                                po::value<string>(&camera_name),
                                "Name of the camera. If a name if passed, it "
                                "has priority over the camera index.")(
        "fps",
        po::value<int>(&fps)->default_value(30)->required(),
        "Frames per secons.")(
        "width",
        po::value<int>(&width)->default_value(640)->required(),
        "Width of the images.")(
        "height",
        po::value<int>(&height)->default_value(480)->required(),
        "Height of the images.")(
        "device",
        po::value<string>(&device_name)->required(),
        "Device to capture images from. It should be either webcam or screen.");

    po::variables_map vm;
    po::store(po::parse_command_line(argc, argv, arguments), vm);
    po::notify(vm);
    if (vm.count("help")) {
        cout << arguments << "\n";
        return 1;
    }

    boost::trim(device_name);

    if (device_name != "webcam" and device_name != "screen") {
        cerr << "Device is not one in the list [webcam, screen, audio]."
             << endl;
        return 1;
    }

    unique_ptr<Device> device;
    if (device_name == "webcam") {
        cout << "Will record from the webcam." << endl;
        if (camera_name.empty()) {
            // Use camera index
            device = make_unique<Webcam>(camera_index, width, height);
        }
        else {
            device = make_unique<Webcam>(camera_name, width, height);
        }
    }
    else {
        cout << "Will record from the screen." << endl;
        device = make_unique<Screen>();
    }

    // Signal handler in case the program is interrupted.
    watch_for_signal();

    try {
        device->turn_on();
        device->start_recording(out_dir, fps, &quit);
    }
    catch (const std::exception& ex) {
        cerr << "[ERROR] Program crashed." << endl;
        cerr << ex.what() << endl;
    }

    return 0;
}
