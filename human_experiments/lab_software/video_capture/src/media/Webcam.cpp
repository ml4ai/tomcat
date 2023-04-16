#pragma ide diagnostic ignored "EndlessLoop"

#include "Webcam.h"

#include <chrono>
#include <iostream>
#include <unistd.h>

#include "fmt/format.h"

#include "common/GeneralException.h"
#include "data_stream/LSLStringStream.h"
#include "external/date.h"

using namespace std;

const int WAIT_UNTIL_READY = 5; // in seconds

//----------------------------------------------------------------------
// Constructors & Destructor
//----------------------------------------------------------------------
Webcam::Webcam(int camera_index, int frame_width, int frame_height) {
    this->init_from_index(camera_index, frame_width, frame_height);
}

Webcam::Webcam(const string& camera_name, int frame_width, int frame_height) {
    if (this->video_device_name_to_index.find(camera_name) ==
        this->video_device_name_to_index.end()) {
        throw GeneralException(
            fmt::format("Camera {} not found.", camera_name));
    }

    int camera_index = this->video_device_name_to_index[camera_name];
    this->init_from_index(camera_index, frame_width, frame_height);
}

Webcam::~Webcam() { this->camera_device.release(); }

//----------------------------------------------------------------------
// Other functions
//----------------------------------------------------------------------
void Webcam::init_from_index(int camera_index,
                             int frame_width,
                             int frame_height) {
    this->camera_device = cv::VideoCapture(camera_index);

    int w = cvRound(this->camera_device.get(cv::CAP_PROP_FRAME_WIDTH));
    int h = cvRound(this->camera_device.get(cv::CAP_PROP_FRAME_HEIGHT));
    cout << fmt::format("[INFO] Maximum resolution is {} x {}.", w, h) << endl;

    this->camera_device.set(cv::CAP_PROP_FRAME_WIDTH, frame_width);
    this->camera_device.set(cv::CAP_PROP_FRAME_HEIGHT, frame_height);

    // Print the resolution to confirm it was set properly
    w = cvRound(this->camera_device.get(cv::CAP_PROP_FRAME_WIDTH));
    h = cvRound(this->camera_device.get(cv::CAP_PROP_FRAME_HEIGHT));
    cout << fmt::format("[INFO] Recording at {} x {}.", w, h) << endl;

    // No need to call open. Camera opens when we create the object. If we call
    // open again, it will reset the pre-defined resolution.
    if (!this->camera_device.isOpened()) {
        throw GeneralException("[ERROR] Cannot open the webcam.");
    }

    // The camera takes a while to fire up. If we start recording immediately
    // the first frames will be empty or dark.
    cout << fmt::format("[INFO] Waiting {} seconds until the webcam is ready.",
                        WAIT_UNTIL_READY)
         << endl;
    sleep(WAIT_UNTIL_READY);
    cout << "[INFO] Webcam is ready." << endl;
}

void Webcam::start_recording(const std::string& out_dir,
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

    cv::Mat img;

    LSLStringStream lsl_stream("Webcam", "webcam", "image_filename", fps);
    lsl_stream.open();

    cout << "[INFO] Started. Recording from the webcam..." << endl;
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

            // Send the string filename to LSL for synchronization with other
            // streams. The image content will be saved to a subdirectory in
            // the experiment folder.
            lsl_stream.send(image_filename);
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

        // Leave the loop so that the class destructor can be called and proper
        // clean-up executed. It does not work if the program is terminated with
        // signal 9.
        if (signal_watcher->load()) {
            cout << "[INFO] Stopped. No longer recording from the webcam."
                 << endl;
            break;
        }
    }
}
