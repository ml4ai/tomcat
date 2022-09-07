from PyQt5 import QtWidgets, QtCore
from pyqtgraph import PlotWidget, plot
import pyqtgraph as pg
import sys  
from pylsl import StreamInlet, resolve_stream
from random import randint                       
import numpy as np
import argparse

class MainWindow(QtWidgets.QMainWindow):

    def __init__(self, *args, **kwargs):
        #initlize streams

        self.streams = resolve_stream('type', 'EEG')
        self.inlet = StreamInlet(self.streams[device_id])

        self.channel_list = ['AFF1h', 'AFF5h' 'F7', 'FC5', 'FC1', 'C3', 'T7', 'TP9', 'CP5', 'CP1', 'Pz', 
        'P3', 'P7', 'PO9', 'O1', 'Oz', 'O2', 'PO10', 'P8', 'P4', 'TP10', 'CP6', 'CP2', 'Cz', 'C4', 'T8', 
        'FC6', 'FC2', 'FCz', 'F8', 'AFF6h', 'AFF2h', 'AUX_GSR', 'AUX_EKG']

        #initialize plots

        super(MainWindow, self).__init__(*args, **kwargs)

        device_name = self.streams[device_id].name()
        self.setWindowTitle(device_name)

        self.graphWidgetLayout = pg.GraphicsLayoutWidget()
        self.graphWidgetLayout.resize(900,2500) 
        self.setCentralWidget(self.graphWidgetLayout)
        
        # Enable antialiasing for prettier plots

        pg.setConfigOptions(antialias=True)

        self.x = [0]
        self.y = [[0] for i in range(len(self.channel_list))] #34 channel data

        self.graphWidgetLayout.setBackground('w')

        self.pen = pg.mkPen(color=(0, 0, 0), width=2) #black

        self.ch = []

        label_style = {"color": (255, 0, 0), "font-size": "10pt"}

        for self.idx, self.channel in enumerate(self.channel_list):
            #create 33 subplots

            self.channel = self.graphWidgetLayout.addPlot(row = self.idx, col = 0)
            #self.channel.showAxes('left', showValues=False)

            if self.idx < 32:
                self.channel.hideAxis('bottom')

            self.channel.setLabel('left', self.channel_list[self.idx], **label_style)
        
            self.ch.append(self.channel)

        self.plots()
    
    def plots(self):
        #draw 

        self.dataLine = [[] for i in range(33)]

        for self.idx, self.ch in enumerate(self.ch):
            self.ch = self.ch.plot(x = self.x, y = self.y[self.idx], pen = self.pen)
    
            self.dataLine[self.idx].append(self.ch)

        self.srate = 500 #500Hz for EEG data
        self.timer = QtCore.QTimer()
        self.timer.setInterval(round(1000/self.srate)) #why? https://stackoverflow.com/questions/59094207/how-to-set-pyqt5-qtimer-to-update-in-specified-interval
        self.timer.timeout.connect(self.update_plot_data)
        self.timer.start()
        

    def update_plot_data(self):
        #update data

        if len(self.x) >= 100:
            self.x = self.x[1:]  # Remove the first x element.
            
            for i in range(len(self.channel_list)):
                self.y[i] = self.y[i][1:]  # Remove the first

        '''
        self.sample,time = self.inlet.pull_sample() #get continuos streams from LSL
        #self.sample = np.random.randint(low = -30, high = 30, size = 34)

        self.x.append(self.x[-1] + 1)  # Add a new value 1 higher than the last.

        for i in range(len(self.channel_list)):
            self.y[i].append(self.sample[i])  # Add a new value.

        for i in range(0,len(self.channel_list)):
            self.dataLine[i][0].setData(self.x, self.y[i])

        '''
        # Get the next chunk of samples from LSL.
        # They were accumulated while we were plotting the previous chunk
        self.sample, time = self.inlet.pull_chunk()

        print(len(self.sample))

        if len(self.sample) > 0:
            # Plot the most recent sample of this chunk. Discard the rest

            # Update the x value according to the number of samples we skipped
            self.x.append(self.x[-1] + len(self.sample))

            # Append the last sample
            for i in range(len(self.channel_list)):
                self.y[i].append(self.sample[-1][i])

            for i in range(0, len(self.channel_list)):
                self.dataLine[i][0].setData(self.x, self.y[i])

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Plotting EEG signals via LSL')
    parser.add_argument("--d", required=True, help="Enter number of EEG devices")
    arg = parser.parse_args()
    device_id = int(arg.d)

    app = QtWidgets.QApplication(sys.argv)
    w = MainWindow()
    w.show()
    sys.exit(app.exec_())
