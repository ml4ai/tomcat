#pip install --upgrade protobuf

import matplotlib.pyplot as plt
import numpy as np
from time import sleep
#from pylsl import StreamInlet, resolve_stream
from IPython.display import display, clear_output
import matplotlib.style as mplstyle
import matplotlib
import os
import multiprocessing
from time import process_time

def plot(ax, fig, buffer1, buffer2, buffer3, p, q, r, channel_list):
            for i in range(0, 60):
                if i<=19:
                    
                    #ax[0].title.set_text('fNIRS Lion')
                    ax[i].axis("off")
                    ax[i].cla()
                    ax[i].plot(buffer1[:,p+40], linewidth='0.5', color="r") #HbO values are channels 40 to 60
                    ax[i].plot(buffer1[:,p+60], linewidth='0.5', color="b") #Hbr values are channels 60 to 80
                    ax[i].set_ylim([-50, 50])
                    ax[i].set_ylabel(channel_list[i], fontsize=8, rotation=0, labelpad=10)
                    ax[i].set_xticks([])
                    ax[i].set_yticks([]) 
                    
                    p += 1  
                    if p == 19:
                        p = 0             
    
                elif i > 19 and i<=39:
                    #ax[20].title.set_text('fNIRS Lion')
                    ax[i].axis("off")
                    ax[i].cla()
                    ax[i].plot(buffer2[:,q+40], linewidth='0.5', color="r") #HbO values are channels 40 to 60
                    ax[i].plot(buffer2[:,q+60], linewidth='0.5', color="b") #Hbr values are channels 60 to 80
                    ax[i].set_ylim([-50, 50])
                    ax[i].set_ylabel(channel_list[i], fontsize=8, rotation=0, labelpad=10)  
                    ax[i].set_xticks([])
                    ax[i].set_yticks([])
                    
                    q += 1
                    if q == 19:
                        q = 0       
                                                                                  
                elif i >= 40:
                    #ax[40].title.set_text('fNIRS Lion')
                    ax[i].axis("off")
                    ax[i].cla()
                    ax[i].plot(buffer3[:,r+40], linewidth='0.5', color="r") #HbO values are channels 40 to 60
                    ax[i].plot(buffer3[:,r+60], linewidth='0.5', color="b") #Hbr values are channels 60 to 80
                    ax[i].set_ylim([-50, 50])
                    ax[i].set_ylabel(channel_list[i], fontsize=8, rotation=0, labelpad=10)
                    ax[i].set_xticks([])
                    ax[i].set_yticks([])
                     
                    r += 1
                    if r == 19:
                       r = 0                 
            
            #plt.subplot_tool()                    
            plt.subplots_adjust(left=0.05, bottom=0.005, right=0.995, top=0.995, wspace=0.200, hspace=0.200)
            fig.tight_layout(pad=3.0)
            fig.canvas.draw()
            fig.canvas.flush_events()    
            clear_output(wait = True)
            display(fig)
                    
def main():            
    #st.markdown("<h1 style='text-align: center; color: grey;'>ToMCAT Physio Visualization</h1>", unsafe_allow_html=True)
    
    #initilize interactive plots
    plt.ion()

    #initlize streams
    #streams = resolve_stream('type', 'NIRS')
    #inlet = StreamInlet(streams[0])

    #make use of buffer to store data from 80 channels
    buffer1 = np.empty([1,80])
    buffer2 = np.empty([1,80])
    buffer3 = np.empty([1,80])

    #channel list as source-detector combinations
    channel_list = ['S1-D1', 'S1-D2', 'S2-D1', 'S2-D3', 'S3-D1', 'S3-D3', 'S3-D4', 'S4-D2', 'S4-D4', 
                    'S4-D5', 'S5-D3', 'S5-D4', 'S5-D6', 'S6-D4', 'S6-D6', 'S6-D7', 'S7-D5', 'S7-D7',
                    'S8-D6', 'S8-D7'] * 3

    #initlize figure 
    mplstyle.use('fast')

    fig, ax = plt.subplots(60)
    fig.set_size_inches(18.5, 10)
    fig.suptitle('ToMCAT Physio Visualization', fontsize=16, y = 0.92)
    
    #fig.set_dpi(10)
    
    p, q, r = 0, 0, 0
    
    while True:
        #sample1,time = inlet1.pull_sample()
        #sample2,time = inlet2.pull_sample()
        #sample3,time = inlet3.pull_sample()
        sample1 = np.random.randint(low = -30, high = 30, size = 81) #development done with random num generator
        sample2 = np.random.randint(low = -30, high = 30, size = 81)
        sample3 = np.random.randint(low = -30, high = 30, size = 81)
        
        buffer1 = np.append(buffer1, np.asarray([sample1[1:]]), axis = 0)
        buffer2 = np.append(buffer2, np.asarray([sample2[1:]]), axis = 0)
        buffer3 = np.append(buffer3, np.asarray([sample3[1:]]), axis = 0)
        
        if buffer1.shape[0] >= 300:
            #Clearning buffer
            buffer1 = np.empty([1,80])
            buffer2 = np.empty([1,80])
            buffer3 = np.empty([1,80])
            
            for i in range(0, 60):
            #Clearning plots
                ax[i].cla()
                
        start = process_time()
        
        p1 = multiprocessing.Process(target = plot(ax, fig, buffer1, buffer2, buffer3, p, q, r, channel_list))
        
        p1.start()
        
        print(process_time() - start)
       
if __name__ == '__main__':
    main()  