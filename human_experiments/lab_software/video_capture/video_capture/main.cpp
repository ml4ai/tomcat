#include <iostream>
#include <string>
#include <fstream>
#include <chrono>

#include <opencv2/highgui.hpp>

// https://stackoverflow.com/questions/12835577/how-to-convert-stdchronotime-point-to-calendar-datetime-string-with-fraction
// date.h: https://howardhinnant.github.io/date/date.html
#include "date.h"

using namespace std;
using namespace cv;


void create_output_directory( const std::filesystem::path p ) {
    std::error_code ec;
    
    if (std::filesystem::exists( p, ec )) {
        cout << "\n\tWrinig frames to already exisiting path: " << p << endl;
    } else {
        cout << "\n\tDirectory: " << p << " does not exists. Creating it.\n";
        
        if (std::filesystem::create_directories( p, ec)) {
            cerr << "\n\t**** ERROR: Directory " << p << " Could not be created. ****\n\n\t\tExiting!";
            return;
        }
    }
}


void webcam(string directory) {
    const std::filesystem::path p(directory);
    create_output_directory( p );

    VideoCapture cap(0);
    cap.open(0); //turn on camera
    
    if ( !cap.isOpened() ) {
        // if not success, exit program
        cout << "\n\t**** ERROR: Cannot open the web cam ****\n" << endl;
        return;
    }
    
    Mat img;
    
    for (unsigned long i = 1; ; i++) {
        auto curr_time = date::floor<std::chrono::milliseconds>(std::chrono::system_clock::now());

        cap.read(img);
        imshow("Webcam", img);

        std::string date_time = date::format("%F %T\n", curr_time).c_str();
        replace( date_time.begin(), date_time.end(), ':', '-');
        replace( date_time.begin(), date_time.end(), ' ', '_');
        replace( date_time.begin(), date_time.end(), '\n', '.'); // Replace the newline character at the end with a dot

        std::filesystem::path file = p;
        file /= std::filesystem::path(to_string(i) + "_" + string(date_time) + "png");
        imwrite(file, img);
        
        waitKey(500);
    }
}


int main(int argc, const char * argv[]) {
    string directory = argv[1];
    webcam( directory );
    return 0;
}
