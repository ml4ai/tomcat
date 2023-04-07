#pragma once

#include <fstream>
#include <string>

#include <opencv2/highgui.hpp>

#include "media/Device.h"

class Webcam : public Device {
  public:
    Webcam(int camera_index,
           int frame_width,
           int frame_height);
    Webcam(const std::string& camera_name,
           int frame_width,
           int frame_height);
    ~Webcam() override;

    Webcam(const Webcam&) = delete;
    Webcam& operator=(const Webcam&) = delete;
    Webcam(Webcam&&) = default;
    Webcam& operator=(Webcam&&) = default;

    /**
     * Starts recording and saves the images to a folder.
     * @param out_dir: directory where images must be saved
     * @param fps: frames per second
     * @param signal_watcher: atomic boolean to check for interruptions
     */
    void start_recording(const std::string& out_dir,
                         int fps,
                         std::atomic<bool>* signal_watcher) override;

  private:
    cv::VideoCapture camera_device;

    void init_from_index(int camera_index, int frame_width, int frame_height);
};
