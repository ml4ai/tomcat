import numpy as np
import cv2
import time as t

cap = cv2.VideoCapture("exvid.mp4")

#fout = open("commentout.txt","w+")
class Comment:
    def __init__(self,comment,time):
        self.time = time
        self.comment = comment
    def __repr__(self):
        return self.comment + "|" + str(self.time)


comments = []
while(True):
    ret, frame = cap.read()
    if not ret:
        break

    cv2.imshow('frame',frame)
    kp = cv2.waitKey(1)
    # p for pause q for quit 
    if kp == ord('p'):
        time = cap.get(cv2.CAP_PROP_POS_MSEC)
        comment = input()
        comments.append(Comment(comment,time))
        cv2.waitKey(0)
    elif kp == ord('q'):
        break
    
print(comments)

cap.release()
cv2.destroyAllWindows()