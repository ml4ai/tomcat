#include <iostream>
#include <string>
#include <fstream>

#include <opencv2/highgui.hpp>

using namespace std;
using namespace cv;

void create_output_directory( string directory ) {
    const std::filesystem::path p(directory);
    std::error_code ec;
    
    if (std::filesystem::exists( p, ec )) {
        cout << "\n\tWrinig frames to already exisiting path: " << directory << endl;
    } else {
        cout << "\n\tDirectory: " << directory << " does not exists. Creating it.\n";
        
        if (std::filesystem::create_directories( directory, ec)) {
            cerr << "\n\t**** ERROR: Directory " << directory << " Could not be created. ****\n\n\t\tExiting!";
            return;
        }
    }
}


void webcam(string directory) {
    create_output_directory( directory );

    VideoCapture cap(0);
    cap.open(0); //turn on camera
    
    if ( !cap.isOpened() ) {
        // if not success, exit program
        cout << "\n\t**** ERROR: Cannot open the web cam ****\n" << endl;
        return;
    }
    
    Mat img;

    while (true) {
        cap.read(img);
        imshow("Webcam", img);
        waitKey(20);
    }
}


int main(int argc, const char * argv[]) {
    string directory = argv[1];
    webcam( directory );
    return 0;
}
