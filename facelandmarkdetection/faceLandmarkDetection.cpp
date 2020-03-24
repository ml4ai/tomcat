//
//  faceLandmarkDetection.cpp
//  opencv
//
//  Created by Adam on 2020/3/21.
//  Copyright © 2020 Adam. All rights reserved.
//

#include <stdio.h>
#include <opencv2/opencv.hpp>
#include <opencv2/face.hpp>
#include "drawLandmarks.hpp"
#include <opencv2/objdetect.hpp>

using namespace std;
using namespace cv;
using namespace cv::face;


int main(int argc,char** argv)
{
 
    // load LBP Face Detector
    CascadeClassifier faceDetector("lbpcascade_frontalface.xml");

    // creat object of facemark
    Ptr<Facemark> facemark = FacemarkLBF::create();

    // load facemark model lbfmodel.yaml
    facemark->loadModel("lbfmodel.yaml");

    // capture the video from the webcam. just for testing
    VideoCapture cam(0);
    
    // store frame and gray
    Mat frame, gray;
    
    // read frame from the webcam
    while(cam.read(frame))
    {
      
      // store the face rectangle.
      vector<Rect> faces;
      // change the video frame to grayscale for input
      cvtColor(frame, gray, COLOR_BGR2GRAY);

      // face detector
      faceDetector.detectMultiScale(gray, faces);
      
      // store points
      vector< vector<Point2f> > landmarks;
      
      // run landmark detector
      bool success = facemark->fit(frame,faces,landmarks);
      
      if(success)
      {
        // if suceess, plot the points.
        for(int i = 0; i < landmarks.size(); i++)
        {
            
            drawLandmarks(frame, landmarks[i]);
            drawFacemarks(frame, landmarks[i], Scalar(0, 0, 255));
        }
    
      }
      imshow("Facial Landmark Detection", frame);
      if (waitKey(1) == 27) break;
      
    }
    return 0;
}
