#pragma ide diagnostic ignored "EndlessLoop"

#include "Webcam.h"

#include <chrono>
#include <iostream>
#include <unistd.h>

#include "fmt/format.h"

#include "common/GeneralException.h"
#include "common/date.h"

using namespace std;

const int WAIT_UNTIL_READY = 5; // in seconds

//----------------------------------------------------------------------
// Constructors & Destructor
//----------------------------------------------------------------------
Webcam::Webcam(int camera_index, int frame_width, int frame_height) {
    this->camera_index = camera_index;
    this->camera_device = cv::VideoCapture(camera_index);
    this->camera_device.set(cv::CAP_PROP_FRAME_WIDTH, frame_width);
    this->camera_device.set(cv::CAP_PROP_FRAME_HEIGHT, frame_height);
}

//----------------------------------------------------------------------
// Other functions
//----------------------------------------------------------------------
void Webcam::turn_on() {
    this->camera_device.open(this->camera_index); // turn on camera

    if (!this->camera_device.isOpened()) {
        throw GeneralException(
            fmt::format("Cannot open the webcam {}", this->camera_index));
    }

    // The camera takes a while to fire up. If we start recording immediately
    // the first frames will be empty or dark.
    cout << fmt::format("Waiting {} seconds until the webcam is ready.",
                        WAIT_UNTIL_READY)
         << endl;
    sleep(WAIT_UNTIL_READY);
    cout << "Webcam is ready." << endl;
}

void Webcam::start_recording(const std::string& out_dir, int fps) {
    const filesystem::path p(out_dir);
    create_output_directory(p);

    int frame_period = 1000 / fps; // Milliseconds
    cout << "\n\tCapturing " << fps << " frames per second.\n";
    cout << "\tA frame is captured every " << frame_period
         << " milliseconds.\n\n";

    cv::Mat img;

    auto prev_frame_time = date::floor<std::chrono::milliseconds>(
        std::chrono::system_clock::now());

    for (unsigned long i = 1;; i++) {
        auto capture_start_time = date::floor<std::chrono::milliseconds>(
            std::chrono::system_clock::now());

        this->camera_device.read(img);
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
            imwrite(file, img);
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
    }
}

void Webcam::create_output_directory(const filesystem::path& p) {
    if (std::filesystem::exists(p)) {
        cout << "\n\tWriting frames to already existing path: " << p << endl;
    }
    else {
        cout << "\n\tDirectory: " << p << " does not exist. Creating it now.\n";
        std::filesystem::create_directories(p);
    }
}

string Webcam::create_image_filename(unsigned long long frame_count,
                                     const string& timestamp,
                                     size_t gap) {
    return to_string(frame_count) + "_" + timestamp + to_string(gap) + ".png";
}
