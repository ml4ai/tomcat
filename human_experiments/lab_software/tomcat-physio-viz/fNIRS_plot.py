from cProfile import label
from PyQt5 import QtWidgets, QtCore
from pyqtgraph import PlotWidget, plot
import pyqtgraph as pg
import sys  # We need sys so that we can pass argv to QApplication
import os
from random import randint
from time import process_time
import numpy as np

class MainWindow(QtWidgets.QMainWindow):

    def __init__(self, *args, **kwargs):

        #initlize streams
        #streams = resolve_stream('type', 'NIRS')
        #inlet = StreamInlet(streams[0])

        self.channel_list = ['S1-D1', 'S1-D2', 'S2-D1', 'S2-D3', 'S3-D1', 'S3-D3', 'S3-D4', 'S4-D2', 'S4-D4', 
                            'S4-D5', 'S5-D3', 'S5-D4', 'S5-D6', 'S6-D4', 'S6-D6', 'S6-D7', 'S7-D5', 'S7-D7',
                            'S8-D6', 'S8-D7']

        #initialize plots
        super(MainWindow, self).__init__(*args, **kwargs)

        self.setWindowTitle("fNIRS device 1")

        self.graphWidgetLayout = pg.GraphicsLayoutWidget()
        self.graphWidgetLayout.resize(900,2500) 
        self.setCentralWidget(self.graphWidgetLayout)
        
        # Enable antialiasing for prettier plots
        pg.setConfigOptions(antialias=True)

        self.x = [0]
        self.y = [[0] for i in range(len(self.channel_list))] #HbO
        self.y1 = [[0] for i in range(len(self.channel_list))] #HbR

        self.graphWidgetLayout.setBackground('w')

        self.pen = pg.mkPen(color=(255, 0, 0), width=5) #red for HbO
        self.pen1 = pg.mkPen(color=(0, 0, 255), width=5) #blue for HbR

        self.ch = []
        self.ch1 = []

        label_style = {"color": (255, 0, 0), "font-size": "14pt"}

        for self.idx, self.channel in enumerate(self.channel_list):
            #create 20 subplots
            self.channel = self.graphWidgetLayout.addPlot(row = self.idx, col = 0)
            #self.channel.showAxes('left', showValues=False)

            if self.idx < 19:
                self.channel.hideAxis('bottom')

            self.channel.setLabel('left', self.channel_list[self.idx], **label_style)
        
            self.ch.append(self.channel)
            self.ch1.append(self.channel)

        self.plots()
    
    def plots(self):
        #draw 
        self.dataLine = [[] for i in range(20)]
        self.dataLine1 = [[] for i in range(20)]

        for self.idx, (self.ch, self.ch1) in enumerate(zip(self.ch, self.ch1)):
            self.ch = self.ch.plot(x = self.x, y = self.y[self.idx], pen = self.pen)
            self.ch1 = self.ch1.plot(x = self.x, y = self.y1[self.idx], pen = self.pen1)
    
            self.dataLine[self.idx].append(self.ch)
            self.dataLine1[self.idx].append(self.ch1)

        self.srate = 10 #10.2Hz for NIRS data
        self.timer = QtCore.QTimer()
        self.timer.setInterval(1000/self.srate) #why? https://stackoverflow.com/questions/59094207/how-to-set-pyqt5-qtimer-to-update-in-specified-interval
        self.timer.timeout.connect(self.update_plot_data)
        self.timer.start()
        

    def update_plot_data(self):
        #update data
        start = process_time()
        if len(self.x) >= 100:
            self.x = self.x[1:]  # Remove the first x element.
            
            for i in range(len(self.channel_list)):
                self.y[i] = self.y[i][1:]  # Remove the first
                self.y1[i] = self.y1[i][1:]  # Remove the first

        #sample,time = inlet1.pull_sample() #get continuos streams from LSL
        self.sample = np.random.randint(low = -30, high = 30, size = 81)

        self.x.append(self.x[-1] + 1)  # Add a new value 1 higher than the last.

        for i in range(len(self.channel_list)):
            self.y[i].append(self.sample[i+40])  # Add a new random value.
            self.y1[i].append(self.sample[i+60])

        for i in range(0,len(self.channel_list)):
            self.dataLine[i][0].setData(self.x, self.y[i])
            self.dataLine1[i][0].setData(self.x, self.y1[i])
        
        print(process_time() - start)

app = QtWidgets.QApplication(sys.argv)
w = MainWindow()
w.show()
sys.exit(app.exec_())