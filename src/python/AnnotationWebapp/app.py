from flask import Flask, render_template, Response
import cv2

app = Flask(__name__)

camera = cv2.VideoCapture('exvid.mp4')

def gen_frames(): 
    #deal with actual video playing times
    pass
# use to stick on page

@app.route('/play_stream')
def play_stream():
    # flask response with video frames
    pass

@app.route('/')
def index():
    return render_template('index.html')


if __name__ == '__main__':
    app.run(debug=True)