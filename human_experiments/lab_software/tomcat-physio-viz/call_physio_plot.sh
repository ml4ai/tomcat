#!/bin/bash

for i in {0..2}
do
    python3 ./fNIRS_plot.py --d $i &
    python3 ./EEG_plot.py --d $i &
done 
exit 1
