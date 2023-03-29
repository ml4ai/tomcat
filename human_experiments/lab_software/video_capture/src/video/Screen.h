#pragma once

#include <fstream>
#include <string>

#include <opencv2/highgui.hpp>

class Screen {
  public:
    Screen(int frame_width, int frame_height);
    ~Screen() = default;

    Screen(const Screen&) = delete;
    Screen& operator=(const Screen&) = delete;
    Screen(Screen&&) = default;
    Screen& operator=(Screen&&) = default;

    /**
     * Turns the webcam on
     */
    void turn_on();

    /**
     * Starts recording and saves the images to a folder.
     * @param out_dir: directory where images must be saved
     * @param fps: frames per second
     */
    void start_recording(const std::string& out_dir, int fps);

  private:
    int camera_index;
    cv::VideoCapture camera_device;

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
