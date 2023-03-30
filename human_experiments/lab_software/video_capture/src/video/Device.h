#pragma once

#include <atomic>
#include <fstream>
#include <string>
#include <unordered_map>
#include <vector>

struct DeviceMetadata {
    std::string name;
    int index;

    DeviceMetadata(std::string name, int index)
        : name(move(name)), index(index) {}
};

class Device {
  public:
    Device(const std::string& unique_id);
    virtual ~Device() = default;

    Device(const Device&) = delete;
    Device& operator=(const Device&) = delete;
    Device(Device&&) = default;
    Device& operator=(Device&&) = default;

    /**
     * Turns the device on
     */
    virtual void turn_on() = 0;

    /**
     * Starts recording from the device and saves the images to a folder.
     *
     * @param out_dir: directory where images must be saved
     * @param fps: frames per second
     */
    virtual void start_recording(const std::string& out_dir,
                                 int fps,
                                 std::atomic<bool>* signal_watcher) = 0;

    /**
     * Uses popen and ffmpeg to list video AVFoundation video devices.
     *
     * @return list of device metadatas
     */
    static std::vector<DeviceMetadata> list_avfoundation_video_devices();

  protected:
    std::string unique_id;

    std::unordered_map<std::string, int> video_device_name_to_index;

    /**
     * Creates an output directory if it does not exist yet.
     *
     * @param p: path to the directory
     */
    static void create_output_directory(const std::filesystem::path& p);

    /**
     * Creates an image filename in an specific format
     *
     * @param frame_count: number of the frame
     * @param timestamp: system timestamp when the image was captured
     * @param gap: how many milliseconds since the last image was captured
     *
     * @return image filename
     */
    static std::string create_image_filename(unsigned long long frame_count,
                                             const std::string& timestamp,
                                             size_t gap);
};
