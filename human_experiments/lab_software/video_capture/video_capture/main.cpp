#include <iostream>
#include <string>
#include <fstream>

#include <opencv2/highgui.hpp>

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

//    while (true) {
    for (int i = 1; ; i++) {
        cap.read(img);
        imshow("Webcam", img);
        
        std::filesystem::path file = p;
        file /= std::filesystem::path(to_string(i) + ".png");
        imwrite(file, img);
        
        waitKey(500);
    }
}


int main(int argc, const char * argv[]) {
    string directory = argv[1];
    webcam( directory );
    return 0;
}
