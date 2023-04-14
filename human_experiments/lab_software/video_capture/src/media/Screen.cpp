#pragma ide diagnostic ignored "EndlessLoop"

#include "Screen.h"

#include <chrono>
#include <iostream>

#include <ApplicationServices/ApplicationServices.h>
#include <fmt/format.h>
#include <opencv2/opencv.hpp>

#include "common/GeneralException.h"
#include "data_stream/LSLStringStream.h"
#include "external/date.h"

using namespace std;

const int WAIT_UNTIL_READY = 10; // in seconds

Screen::Screen(int frame_width, int frame_height)
    : frame_width(frame_width), frame_height(frame_height) {}

void Screen::start_recording(const std::string& out_dir,
                             int fps,
                             atomic<bool>* signal_watcher) {
    const filesystem::path p(out_dir);
    create_output_directory(p);

    int frame_period = 1000 / fps; // Milliseconds
    cout << fmt::format("[INFO] Capturing {} frames per second. A frame is "
                        "captured every {} milliseconds.",
                        fps,
                        frame_period)
         << endl;

    // Full size of the monitor we are recording from
    int max_width = int(CGDisplayPixelsWide(CGMainDisplayID()));
    int max_height = int(CGDisplayPixelsHigh(CGMainDisplayID()));
    cout << fmt::format(
                "[INFO] Maximum resolution is {} x {}.", max_width, max_height)
         << endl;
    cout << fmt::format("[INFO] Recording at {} x {}.",
                        this->frame_width,
                        this->frame_height)
         << endl;

    // CV_8UC4: 8 bit unsigned ints 4 channels -> RGBA
    cv::Mat img(cv::Size(max_width, max_height), CV_8UC4);

    // CV_8UC3: 8 bit unsigned ints 3 channels -> RGB
    // Saving the image with the alpha channel, changes some colors (e.g. red
    // becomes blue). We save the image without the alpha channel.
    cv::Mat final_image(cv::Size(this->frame_width, this->frame_height),
                        CV_8UC3);

    CGColorSpaceRef color_space = CGColorSpaceCreateDeviceRGB();
    CGContextRef context_ref = CGBitmapContextCreate(
        img.data,
        img.cols,
        img.rows,
        8,
        img.step[0],
        color_space,
        kCGImageAlphaPremultipliedLast | kCGBitmapByteOrderDefault);

    // Create LSL stream to synchronize image names and add relevant metadata
    // information
    LSLStringStream lsl_stream("Screen", "screen", "image_filename", fps);
    lsl_stream.open();

    cout << "[INFO] Started. Recording from the screen..." << endl;
    auto prev_frame_time = date::floor<std::chrono::milliseconds>(
        std::chrono::system_clock::now());
    for (unsigned long i = 1;; i++) {
        auto capture_start_time = date::floor<std::chrono::milliseconds>(
            std::chrono::system_clock::now());

        const auto drawing_area = CGRectMake(0, 0, img.cols, img.rows);
        CGImageRef image_ref = CGDisplayCreateImage(CGMainDisplayID());
        CGContextDrawImage(context_ref, drawing_area, image_ref);
        cvtColor(img, img, cv::COLOR_RGBA2BGR);

        // Resize to the desired resolution preserving aspect ratio
        double scale = min((double)this->frame_width / final_image.cols,
                           (double)this->frame_height / final_image.rows);
        cv::Size resized_size(int(final_image.cols * scale),
                              int(final_image.rows * scale));
        cv::resize(img, final_image, resized_size);
        if (!img.empty()) {
            // We need to check if the image is not empty otherwise imwrite
            // will crash.
            std::string date_time =
                date::format("%F_%H-%M-%S.%p~", capture_start_time);

            std::chrono::duration<long, std::milli> between_time =
                capture_start_time - prev_frame_time;
            prev_frame_time = capture_start_time;

            std::filesystem::path file = p;
            string image_filename = create_image_filename(
                i, string(date_time), size_t(between_time.count()));
            file /= std::filesystem::path(image_filename);

            // Send the string filename to LSL for synchronization with other
            // streams. The image content will be saved to a subdirectory in
            // the experiment folder.
            lsl_stream.send(image_filename);
            imwrite(file, final_image);
        }

        auto capture_end_time = date::floor<std::chrono::milliseconds>(
            std::chrono::system_clock::now());
        std::chrono::duration<int, std::milli> capture_duration =
            capture_end_time - prev_frame_time;

        // We cannot just wait for the frame_period time because there'' also
        // the time it takes to capture the image. So, we have to discount that
        // period from the final wait time.
        frame_period > capture_duration.count()
            ? cv::waitKey(frame_period - capture_duration.count())
            : 0;
        CGImageRelease(image_ref);

        // Leave the loop so that the class destructor can be called and proper
        // clean-up executed. It does not work if the program is terminated with
        // signal 9.
        if (signal_watcher->load()) {
            cout << "[INFO] Stopped. No longer recording from the screen."
                 << endl;
            break;
        }
    }
}
