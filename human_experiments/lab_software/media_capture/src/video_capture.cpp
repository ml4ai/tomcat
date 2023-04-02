#include <boost/program_options.hpp>

#include <atomic>
#include <iostream>
#include <stdlib.h>
#include <wordexp.h>

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
    string expanded_dir;
    wordexp_t p;
    string camera_name;
    int camera_index;
    int fps;
    int width;
    int height;

    po::options_description description("Program options");
    description.add_options()(
        "help,h",
        "This program records images from either a webcam or the screen.")(
        "out_dir",
        po::value<string>(&out_dir)->default_value("output_images"),
        "Directory where the images must be saved.")(
        "device",
        po::value<string>(&device_name)->default_value("webcam"),
        "Device to capture images from. It should be either webcam or screen.")(
        "camera_index",
        po::value<int>(&camera_index)->default_value(0),
        "Index of the camera.")("camera_name",
                                po::value<string>(&camera_name),
                                "Name of the camera. If a name if passed, it "
                                "has priority over the camera index.")(
        "fps", po::value<int>(&fps)->default_value(30), "Frames per second.")(
        "width",
        po::value<int>(&width)->default_value(1280),
        "Width of the images.")("height",
                                po::value<int>(&height)->default_value(720),
                                "Height of the images.");

    po::variables_map vm;
    po::store(po::parse_command_line(argc, argv, description), vm);
    po::notify(vm);
    if (vm.count("help")) {
        cout << description << "\n";
        return 1;
    }

    boost::trim(device_name);

    if (device_name != "webcam" and device_name != "screen") {
        cerr << "Device ins not one in the list [webcam, screen, audio]."
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
    else if (device_name == "screen") {
        cout << "Will record from the screen." << endl;
        device = make_unique<Screen>();
    }
    else {
        cout << "Will record audio from the default microphone." << endl;
    }

    switch (wordexp(out_dir.c_str(), &p, 0)) {
        case 0: /* Successful.  */
            expanded_dir = p.we_wordv[0];
            wordfree(&p);
            cout << "Saving images to: " << expanded_dir << endl;
            break;
        case WRDE_NOSPACE:
            /* If the error was WRDE_NOSPACE,
            then perhaps part of the result was allocated.  */
            wordfree(&p);
        default: /* Some other error.  */
            cout << "Error: Could not interprete the path\n";
            return -1;
    }

    // Signal handler in case the program is interrupted.
    watch_for_signal();

    device->turn_on();
    device->start_recording(expanded_dir, fps, &quit);

    return 0;
}
