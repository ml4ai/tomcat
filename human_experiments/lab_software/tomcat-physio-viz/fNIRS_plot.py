import mne 
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from matplotlib.animation import FuncAnimation
import numpy as np
from time import sleep
from pylsl import StreamInlet, resolve_stream
from datetime import datetime
from drawnow import drawnow
from IPython.display import display, clear_output

streams = resolve_stream('type', 'NIRS')
inlet = StreamInlet(streams[0])


plt.ion()

buffer = np.empty([1,80])

fig, ax = plt.subplots(20)

fig.set_size_inches(18.5, 10)
fig.suptitle('fNIRS Lion', fontsize=16)
fig.set_dpi(100)

channel_list = ['S1-D1', 'S1-D2', 'S2-D1', 'S2-D3', 'S3-D1', 'S3-D3', 'S3-D4', 'S4-D2', 'S4-D4', 
                'S4-D5', 'S5-D3', 'S5-D4', 'S5-D6', 'S6-D4', 'S6-D6', 'S6-D7', 'S7-D5', 'S7-D7',
                'S8-D6', 'S8-D7']

while True:
    sample,time = inlet.pull_sample()
    buffer = np.append(buffer, np.asarray([sample[1:]]), axis = 0)
   
    if buffer.shape[0] >= 300:
        print('Clearning buffer')
        buffer = np.empty([1,80])
        
        for i in range(0, 20):
            ax[i].cla()
        
    for i in range(0, 20):
        ax[i].cla()
        ax[i].plot(buffer[:,i+40], linewidth='0.6')
        ax[i].plot(buffer[:,i+60], linewidth='0.6')
        ax[i].set_ylim([-50, 50])
        ax[i].set_ylabel(channel_list[i], fontsize=10, rotation=0, labelpad=20)
        ax[i].set_xticks([])
        ax[i].set_yticks([])
        #ax[i].axis("off")
        
        #ax[i].axes.xaxis.set_visible(False)
        #ax[i].axes.yaxis.set_visible(False)
        
    fig.canvas.draw()
    fig.canvas.flush_events()
    plt.pause(0.0001)
    clear_output(wait = True)
    display(fig)