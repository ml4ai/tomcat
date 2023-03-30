#pragma ide diagnostic ignored "EndlessLoop"

#include "Screen.h"

#include <chrono>
#include <iostream>
#include <unistd.h>

#include <ApplicationServices/ApplicationServices.h>
#include <fmt/format.h>
#include <opencv2/opencv.hpp>

#include "common/GeneralException.h"
#include "common/date.h"

using namespace std;

const int WAIT_UNTIL_READY = 10; // in seconds

//----------------------------------------------------------------------
// Constructors & Destructor
//----------------------------------------------------------------------
Screen::Screen() {}

//----------------------------------------------------------------------
// Other functions
//----------------------------------------------------------------------
void Screen::turn_on() {}

void Screen::start_recording(const std::string& out_dir, int fps) {
    const filesystem::path p(out_dir);
    create_output_directory(p);

    int frame_period = 1000 / fps; // Milliseconds
    cout << "\n\tCapturing " << fps << " frames per second.\n";
    cout << "\tA frame is captured every " << frame_period
         << " milliseconds.\n\n";

    // Full size of the monitor we are recording from
    size_t width = CGDisplayPixelsWide(CGMainDisplayID());
    size_t height = CGDisplayPixelsHigh(CGMainDisplayID());

    // CV_8UC4: 8 bit unsigned ints 4 channels -> RGBA
    cv::Mat img(cv::Size(int(width), int(height)), CV_8UC4);

    // CV_8UC3: 8 bit unsigned ints 3 channels -> RGB
    // Saving the image with the alpha channel, changes some colors (e.g. red
    // becomes blue). We save the image without the alpha channel.
    cv::Mat final_image(cv::Size(int(width), int(height)), CV_8UC3);

    CGColorSpaceRef color_space = CGColorSpaceCreateDeviceRGB();
    CGContextRef context_ref = CGBitmapContextCreate(
        img.data,
        img.cols,
        img.rows,
        8,
        img.step[0],
        color_space,
        kCGImageAlphaPremultipliedLast | kCGBitmapByteOrderDefault);

    auto prev_frame_time = date::floor<std::chrono::milliseconds>(
        std::chrono::system_clock::now());

    for (unsigned long i = 1;; i++) {
        auto capture_start_time = date::floor<std::chrono::milliseconds>(
            std::chrono::system_clock::now());

        CGImageRef image_ref = CGDisplayCreateImage(CGMainDisplayID());
        CGContextDrawImage(
            context_ref, CGRectMake(0, 0, int(width), int(height)), image_ref);
        cvtColor(img, final_image, cv::COLOR_RGBA2BGR);
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
    }
}
