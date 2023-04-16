#!/bin/bash

NIRS=("leopard_0171" "leopard_0171" "tiger_0239")
EEG=("actiCHamp-21010477" "actiCHamp-21020492" "actiCHamp-20010205")
for i in $(seq 0 $((${#NIRS[@]}-1)))
do
    python3 ./fNIRS_plot.py --d ${NIRS[$i]} &
    python3 ./EEG_plot.py --d ${EEG[$i]} &
done 
exit 1
