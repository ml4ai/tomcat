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

fig, ax = plt.subplots(40)

fig.set_size_inches(18.5, 10)
fig.set_dpi(100)

while True:
    
   # if buffer.shape[0] >= 100:
    #    print('Clearning buffer')
     #   fig.clf()
      #  buffer = np.empty([1,80])

        
    for i in range(1, 40):
        sample,time = inlet.pull_sample()
        buffer = np.append(buffer, np.asarray([sample[1:]]), axis = 0)
        ax[i].cla()
        ax[i].plot(buffer[:,i])
        ax[i].axes.xaxis.set_visible(False)
        ax[i].axes.yaxis.set_visible(False)
        
    fig.canvas.draw()
    fig.canvas.flush_events()
    plt.pause(0.0001)
    clear_output(wait = True)
    
    display(fig)