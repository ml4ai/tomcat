#include "unistd.h"
#include <chrono>
#include <fstream>
#include <iostream>
#include <string>

#include <opencv2/highgui.hpp>

// https://stackoverflow.com/questions/12835577/how-to-convert-stdchronotime-point-to-calendar-datetime-string-with-fraction
// date.h: https://howardhinnant.github.io/date/date.html
#include "date.h"

using namespace std;
using namespace cv;

const int WAIT_TIME_BEFORE_RECORDING = 5

void create_output_directory(const std::filesystem::path p) {
    std::error_code ec;

    if (std::filesystem::exists(p, ec)) {
        cout << "\n\tWriting frames to already existing path: " << p << endl;
    }
    else {
        cout << "\n\tDirectory: " << p << " does not exist. Creating it now.\n";

        if (std::filesystem::create_directories(p, ec)) {
            cerr << "\n\t**** ERROR: Directory " << p
                 << " Could not be created. ****\n\n\t\tExiting!";
            return;
        }
    }
}

void webcam(const string directory,
            const int frames_per_minute = 2,
            const int webcam_id = 0) {
    const std::filesystem::path p(directory);
    create_output_directory(p);

    int frame_period = 1000 / frames_per_minute; // Milliseconds
    cout << "\n\tCapturing " << frames_per_minute << " frames per minute.\n";
    cout << "\tA frame is captured every " << frame_period
         << " milliseconds.\n\n";

    VideoCapture cap(webcam_id);
    cap.open(0); // turn on camera

    if (!cap.isOpened()) {
        // if not success, exit program
        cout << "\n\t**** ERROR: Cannot open the web cam ****\n" << endl;
        return;
    }

    // The camera takes a while to fire up. If we start recording immediately
    // the first frames will be empty or dark.
    sleep(WAIT_TIME_BEFORE_RECORDING);

    Mat img;

    auto prev_frame_time = date::floor<std::chrono::milliseconds>(
        std::chrono::system_clock::now());

    for (unsigned long i = 1;; i++) {
        auto capture_start_time = date::floor<std::chrono::milliseconds>(
            std::chrono::system_clock::now());

        cap.read(img);
        if (!img.empty()) {
            // We need to check if the image is not empty because imshow crashes
            // when that happens.
            imshow("Webcam", img);

            std::string date_time =
                date::format("%F_%H-%M-%S.%p~", capture_start_time).c_str();

            std::chrono::duration<long, std::milli> between_time =
                capture_start_time - prev_frame_time;
            prev_frame_time = capture_start_time;

            std::filesystem::path file = p;
            file /=
                std::filesystem::path(to_string(i) + "_" + string(date_time) +
                                      to_string(between_time.count()) + ".png");
            imwrite(file, img);
        }

        auto capture_end_time = date::floor<std::chrono::milliseconds>(
            std::chrono::system_clock::now());
        std::chrono::duration<int, std::milli> capture_duration =
            capture_end_time - prev_frame_time;

        frame_period > capture_duration.count()
            ? waitKey(frame_period - capture_duration.count())
            : 0;
    }
}

int main(int argc, const char* argv[]) {
    const string directory = argv[1];
    int frames_per_minute = 2;
    int webcam_id = 0;

    std::size_t pos{};

    try {
        frames_per_minute = std::stoi(argv[2], &pos);
    }
    catch (std::invalid_argument const& ex) {
        cout << "\n\t**** ERROR: Invalid number of frames per minute entered: "
             << argv[2] << endl;
        return -1;
    }
    catch (std::out_of_range const& ex) {
        cout << "\n\t**** ERROR: Frames per minute entered is too large: "
             << argv[2] << endl;
        return -1;
    }

    if (argc > 3) {
        try {
            webcam_id = std::stoi(argv[3], &pos);
        }
        catch (std::invalid_argument const& ex) {
            cout << "\n\t**** ERROR: Invalid webcam ID entered: " << argv[3]
                 << endl;
            return -1;
        }
        catch (std::out_of_range const& ex) {
            cout << "\n\t**** ERROR: webcam ID entered is too large: "
                 << argv[3] << endl;
            return -1;
        }
    }

    webcam(directory, frames_per_minute, webcam_id);

    return 0;
}
