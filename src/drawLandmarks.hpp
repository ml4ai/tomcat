//
//  drawLandmarks.hpp
//  opencv
//
//  Created by Adam on 2020/3/22.
//  Copyright © 2020 Adam. All rights reserved.
//

#ifndef drawLandmarks_h
#define drawLandmarks_h


#endif /* drawLandmarks_h */
#ifndef _renderFace_H_
#define _renderFace_H_

#include <iostream>
#include <opencv2/opencv.hpp>

using namespace cv;
using namespace std;

#define COLOR Scalar(255, 200,0)

void drawPolyline
(
  Mat &im,
  const vector<Point2f> &landmarks,
  const int start,
  const int end,
  bool isClosed = false
)
{
    vector <Point> points;
    for (int i = start; i <= end; i++)
    {
        points.push_back(cv::Point(landmarks[i].x, landmarks[i].y));
    }


    polylines(im, points, isClosed, COLOR, 2, 16);
    
}

// 绘制人脸关键点
void drawLandmarks(Mat &im, vector<Point2f> &landmarks)
{

    if (landmarks.size() == 68)
    {
      drawPolyline(im, landmarks, 0, 16);           // Jaw line
      drawPolyline(im, landmarks, 17, 21);          // Left eyebrow
      drawPolyline(im, landmarks, 22, 26);          // Right eyebrow
      drawPolyline(im, landmarks, 27, 30);          // Nose bridge
      drawPolyline(im, landmarks, 30, 35, true);    // Lower nose
      drawPolyline(im, landmarks, 36, 41, true);    // Left eye
      drawPolyline(im, landmarks, 42, 47, true);    // Right Eye
      drawPolyline(im, landmarks, 48, 59, true);    // Outer lip
      drawPolyline(im, landmarks, 60, 67, true);    // Inner lip
    }
    else
    {

        for(int i = 0; i < landmarks.size(); i++)
        {
            circle(im,landmarks[i],3, COLOR, FILLED);
        }
    }
    
}

#endif // _renderFace_H_
