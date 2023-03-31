#pragma once

#include <fstream>
#include <string>

#include <opencv2/highgui.hpp>

#include "video/Device.h"

class Webcam : public Device {
  public:
    Webcam(const std::string& unique_id,
           int camera_index,
           int frame_width,
           int frame_height);
    Webcam(const std::string& unique_id,
           const std::string& camera_name,
           int frame_width,
           int frame_height);
    ~Webcam() override;

    Webcam(const Webcam&) = delete;
    Webcam& operator=(const Webcam&) = delete;
    Webcam(Webcam&&) = default;
    Webcam& operator=(Webcam&&) = default;

    /**
     * Turns the webcam on
     */
    void turn_on() override;

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
    int camera_index = 0;
    cv::VideoCapture camera_device;
};
