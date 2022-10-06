#include <iostream>
#include <opencv2/highgui.hpp>

using namespace std;
using namespace cv;


void webcam() {
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
    webcam();
    return 0;
}
