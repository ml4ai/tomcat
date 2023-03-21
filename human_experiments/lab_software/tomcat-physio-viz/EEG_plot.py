from PyQt5 import QtWidgets, QtCore
import pyqtgraph as pg
import sys
from pylsl import StreamInlet, resolve_stream
import argparse

class MainWindow(QtWidgets.QMainWindow):
    def __init__(self, *args, **kwargs):
        # initialize streams

        self.host_name = {
            "actiCHamp-20010205": "Tiger - EEG(200 10 205)",
            "actiCHamp-21010477": "Lion - EEG(210 10 477)", #Lions EEG amp is replaced with cheetah's amp.
            "actiCHamp-21020492": "Leopard - EEG(210 20 492)",
        }

        self.streams = resolve_stream("type", "EEG")
        self.inlet = StreamInlet(self.streams[device_id])

        self.channel_list = [
            "AFF1h",
            "AFF5h",
            "F7",
            "FC5",
            "FC1",
            "C3",
            "T7",
            "TP9",
            "CP5",
            "CP1",
            "Pz",
            "P3",
            "P7",
            "PO9",
            "O1",
            "Oz",
            "O2",
            "PO10",
            "P8",
            "P4",
            "TP10",
            "CP6",
            "CP2",
            "Cz",
            "C4",
            "T8",
            "FC6",
            "FC2",
            "FCz",
            "F8",
            "AFF6h",
            "AFF2h",
            "GSR",
            "EKG",
        ] #AUX_EKG = EKG and AUX_GSR = GSR

        # Since 11/22 the lab decided to use the following channels:

        self.channels_used = [
            "AFF1h",
            "F7",
            "FC5",
            "C3",
            "T7",
            "TP9",
            "Pz",
            "P3",
            "P7",
            "O1",
            "O2",
            "P8",
            "P4",
            "TP10",
            "Cz",
            "C4",
            "T8",
            "FC6",
            "FCz",
            "F8",
            "AFF2h",
            "GSR",
            "EKG",
        ]

        # Get the index of channel that are being used.

        self.channels_used_index = [
            self.channel_list.index(ch)
            for ch in self.channels_used
            if ch in self.channel_list
        ]

        # initialize plots

        super(MainWindow, self).__init__(*args, **kwargs)

        device_name = self.streams[device_id].name()
        for stream_name, stream_name_with_id in self.host_name.items():
            if device_name in stream_name:
                self.setWindowTitle(stream_name_with_id)

        self.graphWidgetLayout = pg.GraphicsLayoutWidget()
        self.graphWidgetLayout.resize(900, 2500)
        self.setCentralWidget(self.graphWidgetLayout)

        # Enable antialiasing for prettier plots

        pg.setConfigOptions(antialias=True)

        self.graphWidgetLayout.setBackground("w")

        self.pen = pg.mkPen(color=(0, 0, 0), width=1)  # black

        self.ch = []

        label_style = {"color": "black", "font-size": "8pt"}

        self.srate = 500  # 500Hz for EEG data
        self.timer = QtCore.QTimer()
        # why? https://stackoverflow.com/questions/59094207/how-to-set-pyqt5-qtimer-to-update-in-specified-interval
        self.timer.setInterval(round(1000 / self.srate))

        n_channels = len(self.channels_used)

        self.x = [0]
        self.y = [[0] for _ in range(n_channels)]  # 23 channel data

        self.dataLine = [[] for _ in range(n_channels)]

        for self.idx, self.channel in enumerate(self.channels_used):
            # create 33 subplots

            self.channel = self.graphWidgetLayout.addPlot(row=self.idx, col=0)
            # self.channel.showAxes('left', showValues=False)

            if self.idx < n_channels - 1:
                self.channel.hideAxis("bottom")

            self.channel.setLabel("left", self.channels_used[self.idx], **label_style)

            self.ch.append(self.channel)

        self.plots()

    def plots(self):
        # draw

        for self.idx, self.ch in enumerate(self.ch):
            self.ch = self.ch.plot(x=self.x, y=self.y[self.idx], pen=self.pen)

            self.dataLine[self.idx].append(self.ch)

        self.timer.timeout.connect(self.update_plot_data)
        self.timer.start()

    def update_plot_data(self):
        # update data

        if len(self.x) >= 100:
            self.x = self.x[1:]  # Remove the first x element.

            for i in range(len(self.channels_used)):
                self.y[i] = self.y[i][1:]  # Remove the first

        # Get the next chunk of samples from LSL.
        # They were accumulated while we were plotting the previous chunk
        sample, time = self.inlet.pull_chunk()

        if len(sample) > 0:
            # Plot the most recent sample of this chunk. Discard the rest

            # Update the x value according to the number of samples we skipped
            self.x.append(self.x[-1] + len(sample))

            # Append the last sample
            for i in range(len(self.channels_used)):
                self.y[i].append(sample[-1][i])

            for i in range(0, len(self.channels_used)):
                self.dataLine[i][0].setData(self.x, self.y[i])

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Plotting EEG signals via LSL")
    parser.add_argument("--d", required=True, help="Enter number of EEG devices")
    arg = parser.parse_args()
    device_id = int(arg.d)

    app = QtWidgets.QApplication(sys.argv)
    w = MainWindow()
    w.show()
    sys.exit(app.exec_())
