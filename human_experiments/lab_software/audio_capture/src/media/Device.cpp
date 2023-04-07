#include "Device.h"

#include <array>
#include <iostream>
#include <regex>
#include <sstream>

#include "common/Popen.h"

using namespace std;

Device::Device() {
    for (const auto& device : list_avfoundation_video_devices()) {
        video_device_name_to_index[device.name] = device.index;
    }

    cout << "\nDevice List\n" << endl;
    for (const auto& [name, index] : this->video_device_name_to_index){
        cout << name << ": " << index << endl;
    }
    cout << "******************************" << endl;
}

void Device::create_output_directory(const filesystem::path& p) {
    if (std::filesystem::exists(p)) {
        cout << "[INFO] Writing frames to already existing path: " << p << endl;
    }
    else {
        cout << "[INFO] Directory: " << p << " does not exist. Creating it now." << endl;
        std::filesystem::create_directories(p);
    }
}

string Device::create_image_filename(unsigned long long frame_count,
                                     const string& timestamp,
                                     size_t gap) {
    return to_string(frame_count) + "_" + timestamp + to_string(gap) + ".png";
}

std::vector<DeviceMetadata> Device::list_avfoundation_video_devices() {
    string ffmpeg_device_list =
        Popen::exec("ffmpeg -hide_banner -f avfoundation  "
                    "-list_devices true -i dummy 2>&1");
    std::stringstream ss(ffmpeg_device_list);
    std::string token;
    regex device_type_regex(".*AVFoundation video devices:$");
    regex device_regex("\\[(\\d+)\\]\\s+(.*$)");
    smatch matches;

    cout << "\n******************" << endl;
    cout << "ffmpeg device list" << endl;
    cout << "******************\n" << endl;

    vector<DeviceMetadata> devices;
    bool row_contains_device = false;
    while (std::getline(ss, token, '\n')) {
        cout << token << endl;

        if (row_contains_device) {
            if (std::regex_search(token, matches, device_regex)) {
                // The first regex group is the index and the second the name.
                devices.push_back({matches[2], stoi(matches[1])});
            }
            else {
                // Video device list ended.
                break;
            }
        }

        if (std::regex_search(token, matches, device_type_regex)) {
            // The subsequent rows should contain the device list.
            row_contains_device = true;
        }
    }

    cout << "\n******************" << endl;

    return devices;
}
