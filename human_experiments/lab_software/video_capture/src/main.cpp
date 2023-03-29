#include "video/Webcam.h"

int main(int argc, const char* argv[]) {
    Webcam webcam(0, 1280, 720);

    webcam.turn_on();
    webcam.start_recording("../images/", 10);

    return 0;
}
