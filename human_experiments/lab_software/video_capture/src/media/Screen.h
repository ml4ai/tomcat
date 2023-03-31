#pragma once

#include <fstream>
#include <string>

#include <opencv2/highgui.hpp>

#include "video/Device.h"

#include <iostream>

class Screen : public Device {
  public:
    Screen(const std::string& unique_id);
    ~Screen() override = default;

    Screen(const Screen&) = delete;
    Screen& operator=(const Screen&) = delete;
    Screen(Screen&&) = default;
    Screen& operator=(Screen&&) = default;

    /**
     * Does initial setup for screen recording.
     */
    void turn_on() override;

    /**
     * Starts recording and saves the images to a folder.
     *
     * @param out_dir: directory where images must be saved
     * @param fps: frames per second
     * @param signal_watcher: atomic boolean to check for interruptions
     */
    void start_recording(const std::string& out_dir,
                         int fps,
                         std::atomic<bool>* signal_watcher) override;
};
