#!/bin/bash

for i in {1..3}
do
    python3 ./fNIRS_plot.py --d $i &
done 
exit 1
