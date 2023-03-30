#pragma once

#include <fstream>
#include <string>

#include <opencv2/highgui.hpp>

#include "video/Device.h"

class Screen : public Device {
  public:
    Screen();
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
     */
    void start_recording(const std::string& out_dir, int fps) override;
};
