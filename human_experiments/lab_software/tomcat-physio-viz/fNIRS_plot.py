#pip install --upgrade protobuf

import streamlit as st
import matplotlib.pyplot as plt
import numpy as np
from time import sleep
#from pylsl import StreamInlet, resolve_stream
from IPython.display import display, clear_output
import matplotlib.style as mplstyle
import os
                
def main():                
    st.markdown("<h1 style='text-align: center; color: grey;'>ToMCAT Physio Visualization</h1>", unsafe_allow_html=True)
    
    #initilize interactive plots
    plt.ion()

    #initlize streams
    #streams = resolve_stream('type', 'NIRS')
    #inlet = StreamInlet(streams[0])

    #make use of buffer to store data from 80 channels
    buffer = np.empty([1,80])

    #channel list as source-detector combinations
    channel_list = ['S1-D1', 'S1-D2', 'S2-D1', 'S2-D3', 'S3-D1', 'S3-D3', 'S3-D4', 'S4-D2', 'S4-D4', 
                    'S4-D5', 'S5-D3', 'S5-D4', 'S5-D6', 'S6-D4', 'S6-D6', 'S6-D7', 'S7-D5', 'S7-D7',
                    'S8-D6', 'S8-D7']

    #initlize figure 
    mplstyle.use('fast')

    fig, ax = plt.subplots(20)
    fig.set_size_inches(18.5, 10)
    fig.suptitle('fNIRS Lion', fontsize=16, y = 0.92)
    fig.set_dpi(100)

    fig1, ax1 = plt.subplots(20)
    fig1.set_size_inches(18.5, 10)
    fig1.suptitle('fNIRS Tiger', fontsize=16, y = 0.92)
    fig1.set_dpi(100)

    fig2, ax2 = plt.subplots(20)
    fig2.set_size_inches(18.5, 10)
    fig2.suptitle('fNIRS Leopard', fontsize=16, y = 0.92)
    fig2.set_dpi(100)
    
    # creating a single-element container
    placeholder = st.empty()
    placeholder1 = st.empty()
    placeholder2 = st.empty()

    while True:
        #sample,time = inlet.pull_sample()
        sample = np.random.randint(low = -30, high = 30, size = 81) #development done with random num
        buffer = np.append(buffer, np.asarray([sample[1:]]), axis = 0)
        
        if buffer.shape[0] >= 300:
            #('Clearning buffer')
            buffer = np.empty([1,80])
        
            for i in range(0, 20):
                ax[i].cla()
 
        with placeholder.container():
            for i in range(0, 20):
                ax[i].cla()
                ax[i].plot(buffer[:,i+40], linewidth='0.6', color="r")
                ax[i].plot(buffer[:,i+60], linewidth='0.6', color="b")
                ax[i].set_ylim([-50, 50])
                ax[i].set_ylabel(channel_list[i], fontsize=10, rotation=0, labelpad=20)
                #ax[i].set_xticks([])
                ax[i].set_yticks([])

            st.pyplot(fig)

        with placeholder1.container():
            for i in range(0, 20):
                ax1[i].cla()
                ax1[i].plot(buffer[:,i+40], linewidth='0.6', color="r")
                ax1[i].plot(buffer[:,i+60], linewidth='0.6', color="b")
                ax1[i].set_ylim([-50, 50])
                ax1[i].set_ylabel(channel_list[i], fontsize=10, rotation=0, labelpad=20)
                #ax1[i].set_xticks([])
                ax1[i].set_yticks([])

            st.pyplot(fig1)

        with placeholder2.container():
            for i in range(0, 20):
                ax2[i].cla()
                ax2[i].plot(buffer[:,i+40], linewidth='0.6', color="r")
                ax2[i].plot(buffer[:,i+60], linewidth='0.6', color="b")
                ax2[i].set_ylim([-50, 50])
                ax2[i].set_ylabel(channel_list[i], fontsize=10, rotation=0, labelpad=20)
                #ax2[i].set_xticks([])
                ax2[i].set_yticks([])

            st.pyplot(fig2)
       
        #plt.pause(0.0001)
        clear_output(wait = True)     
         
if __name__ == '__main__':
    main()  