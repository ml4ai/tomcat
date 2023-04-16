from PyQt5 import QtWidgets, QtCore
import pyqtgraph as pg
import sys
from pylsl import StreamInlet, resolve_stream
import argparse

class MainWindow(QtWidgets.QMainWindow):
    def __init__(self, *args, **kwargs):
        self.channel_list = [
            "S1-D1",
            "S1-D2",
            "S2-D1",
            "S2-D3",
            "S3-D1",
            "S3-D3",
            "S3-D4",
            "S4-D2",
            "S4-D4",
            "S4-D5",
            "S5-D3",
            "S5-D4",
            "S5-D6",
            "S6-D4",
            "S6-D6",
            "S6-D7",
            "S7-D5",
            "S7-D7",
            "S8-D6",
            "S8-D7",
        ]

        # initialize plots

        super(MainWindow, self).__init__(*args, **kwargs)

        # initialize streams
        self.host_name = {
            "tiger_0239": "Tiger - fNIRS(2101_0239_A)",
            "lion_0297": "Lion - fNIRS(2118_0297_A)",
            "leopard_0171": "Leopard - fNIRS(2010_0171_A)",
        }

        self.streams = resolve_stream()

        for i in range(len(self.streams)):
            if self.streams[i].type() == 'NIRS' and self.streams[i].name() == device_id:
                 self.inlet = StreamInlet(self.streams[i])

        for stream_name, stream_name_with_id in self.host_name.items():
            if device_id in stream_name:
                self.setWindowTitle(stream_name_with_id)

        self.graphWidgetLayout = pg.GraphicsLayoutWidget()
        self.graphWidgetLayout.resize(900, 2500)
        self.setCentralWidget(self.graphWidgetLayout)

        # Enable antialiasing for prettier plots

        pg.setConfigOptions(antialias=True)

        self.graphWidgetLayout.setBackground("w")

        self.pen = pg.mkPen(color=(255, 0, 0), width=2)  # red for HbO
        self.pen1 = pg.mkPen(color=(0, 0, 255), width=2)  # blue for HbR

        self.ch = []
        self.ch1 = []

        label_style = {"color": (255, 0, 0), "font-size": "10pt"}

        self.srate = 10  # 10.2Hz for NIRS data
        self.timer = QtCore.QTimer()
        # why? https://stackoverflow.com/questions/59094207/how-to-set-pyqt5-qtimer-to-update-in-specified-interval
        self.timer.setInterval(round(1000 / self.srate))

        n_channels = len(self.channel_list)

        self.x = [0]
        self.y = [[0] for _ in range(n_channels)]  # HbO
        self.y1 = [[0] for _ in range(n_channels)]  # HbR

        self.dataLine = [[] for _ in range(n_channels)]
        self.dataLine1 = [[] for _ in range(n_channels)]

        for self.idx, self.channel in enumerate(self.channel_list):
            # create 20 subplots

            self.channel = self.graphWidgetLayout.addPlot(row=self.idx, col=0)
            # self.channel.showAxes('left', showValues=False)

            if self.idx < n_channels - 1:
                self.channel.hideAxis("bottom")

            self.channel.setLabel("left", self.channel_list[self.idx], **label_style)

            self.ch.append(self.channel)
            self.ch1.append(self.channel)

        self.plots()

    def plots(self):
        # draw

        for self.idx, (self.ch, self.ch1) in enumerate(zip(self.ch, self.ch1)):
            self.ch = self.ch.plot(x=self.x, y=self.y[self.idx], pen=self.pen)
            self.ch1 = self.ch1.plot(x=self.x, y=self.y1[self.idx], pen=self.pen1)

            self.dataLine[self.idx].append(self.ch)
            self.dataLine1[self.idx].append(self.ch1)

        self.timer.timeout.connect(self.update_plot_data)
        self.timer.start()

    def update_plot_data(self):
        # update data

        if len(self.x) >= 100:
            self.x = self.x[1:]  # Remove the first x element.

            for i in range(len(self.channel_list)):
                self.y[i] = self.y[i][1:]  # Remove the first
                self.y1[i] = self.y1[i][1:]  # Remove the first

        # Get the next chunk of samples from LSL.
        # They were accumulated while we were plotting the previous chunk
        sample, time = self.inlet.pull_chunk()

        if len(sample) > 0:
            # Plot the most recent sample of this chunk. Discard the rest

            # Update the x value according to the number of samples we skipped
            self.x.append(self.x[-1] + len(sample))

            # Append the last sample
            for i in range(len(self.channel_list)):
                self.y[i].append(sample[-1][i + 40])
                self.y1[i].append(sample[-1][i + 60])

            for i in range(0, len(self.channel_list)):
                self.dataLine[i][0].setData(self.x, self.y[i])
                self.dataLine1[i][0].setData(self.x, self.y1[i])


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Plotting fNIRS signals via LSL")
    parser.add_argument("--d", required=True, help="Enter number of fNIRS devices")
    arg = parser.parse_args()
    device_id = str(arg.d)

    app = QtWidgets.QApplication(sys.argv)
    w = MainWindow()
    w.show()
    sys.exit(app.exec_())
