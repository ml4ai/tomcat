Software
========

Software setup on windows machine
---------------------------------

Impedance check (Brain Vision Recorder app)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

1. start windows machine
2. open virtual box application

   -  press start (green arrow) on one, then wait for it to start up.
   -  then, press start on the second one

      -  you should have two clones open, plus the original windows

3. connect USB on the clones (right click on USB symbol to open,
   left-click to connect)

   -  each clone needs exactly one actichamp & one sentinel connected
   -  disconnect if there is more than one connected

      -  trouble shoot: make sure one of the clones isn’t connected to
         all actichamps/sentinels

4. open Brain Vision Recorder on all three machines

   -  click “amplifier” tab to check which serial number is connected to
      this particular app (lion, tiger, leopard)
   -  proceed with impedance check by clicking lightbulb symbol

5. once impedance check is complete, press “stop” button in Brain Vision
   Recorder app, close app

   -  DO NOT close the virtual machine using the “X” button, ALWAYS shut
      down

6. shut down each of the two virtual machines one by one like a normal
   computer (click on windows symbol, select “shutdown”)

LSL streaming setup (LSL app)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

1. open LSL actichamp three times (3 windows side by side)

   -  copy &paste electrode labels from the “electrodes” file on the
      desktop into the app

2. enter one serial numbers into one of the apps, until all three amps
   are listed

   -  click “link” to connect

      -  start out with leopard
      -  try order of connected amp to clone 2, clone 1, original
         windows NOTE: if one of the three fails to connect, you have to
         disconnect all amps, close all LSL windows, and open three news
         LSL windows.

   -  try different orders of connecting the amps

| ``actichamp = Brain Vision Amplifier``
| ``Sentinel = license key for application``
