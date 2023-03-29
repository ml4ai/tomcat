#include <boost/program_options.hpp>

#include <iostream>

#include "video/Screen.h"
#include "video/Webcam.h"

using namespace std;
namespace po = boost::program_options;

int main(int argc, const char* argv[]) {
    string device;
    string out_dir;
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
        po::value<int>(&camera_index)->default_value(0)->required(),
        "Index of the camera.")(
        "fps",
        po::value<int>(&fps)->default_value(30)->required(),
        "Frames per secons.")(
        "width",
        po::value<int>(&width)->default_value(1280)->required(),
        "Width of the images.")(
        "height",
        po::value<int>(&height)->default_value(720)->required(),
        "Height of the images.")(
        "device",
        po::value<string>(&device)->required(),
        "Device to capture images from. It should be either webcam or screen.");

    po::variables_map vm;
    po::store(po::parse_command_line(argc, argv, arguments), vm);
    po::notify(vm);
    if (vm.count("help")) {
        cout << arguments << "\n";
        return 1;
    }

    if (device != "webcam" and device != "screen") {
        cerr << "Device ins not one in the list [webcam, screen]." << endl;
    }

    if (device == "webcam") {
        cout << "Will record from the webcam" << endl;
        Webcam webcam(camera_index, width, height);
        webcam.turn_on();
        webcam.start_recording(out_dir, fps);
    }
    else {
        cout << "Will record from the screen" << endl;
        Screen screen(width, height);
        screen.start_recording(out_dir, fps);
    }

    return 0;
}
