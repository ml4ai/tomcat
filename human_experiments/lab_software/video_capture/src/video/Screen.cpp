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
Screen::Screen(int frame_width, int frame_height) {}

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

    size_t width = CGDisplayPixelsWide(CGMainDisplayID());
    size_t height = CGDisplayPixelsHigh(CGMainDisplayID());

    cv::Mat img(cv::Size(width, height), CV_8UC4);
    cv::Mat bgrim(cv::Size(width, height), CV_8UC3);
    cv::Mat resizedim(cv::Size(width, height), CV_8UC3);

    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGContextRef contextRef = CGBitmapContextCreate(
        img.data,
        img.cols,
        img.rows,
        8,
        img.step[0],
        colorSpace,
        kCGImageAlphaPremultipliedLast | kCGBitmapByteOrderDefault);

    auto prev_frame_time = date::floor<std::chrono::milliseconds>(
        std::chrono::system_clock::now());

    for (unsigned long i = 1;; i++) {
        auto capture_start_time = date::floor<std::chrono::milliseconds>(
            std::chrono::system_clock::now());

        CGImageRef imageRef = CGDisplayCreateImage(CGMainDisplayID());
        CGContextDrawImage(
            contextRef, CGRectMake(0, 0, width, height), imageRef);
        cvtColor(img, bgrim, cv::COLOR_RGBA2BGR);
        if (!bgrim.empty()) {
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
            imwrite(file, bgrim);
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
        CGImageRelease(imageRef);
    }
}

void Screen::create_output_directory(const filesystem::path& p) {
    if (std::filesystem::exists(p)) {
        cout << "\n\tWriting frames to already existing path: " << p << endl;
    }
    else {
        cout << "\n\tDirectory: " << p << " does not exist. Creating it now.\n";
        std::filesystem::create_directories(p);
    }
}

string Screen::create_image_filename(unsigned long long frame_count,
                                     const string& timestamp,
                                     size_t gap) {
    return to_string(frame_count) + "_" + timestamp + to_string(gap) + ".png";
}
