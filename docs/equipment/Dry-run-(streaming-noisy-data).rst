How to stream noisy data (= no participant needed)
==================================================

eyetracker (iMAC)
~~~~~~~~~~~~~~~~~

1. connect device via USB
2. open “pupil capture” application on desktop of iMAC
3. hit the record button (R on left bottom of screen)

fNIRS (iMAC)
~~~~~~~~~~~~

1. connect sources & detectors to NIRSport (white box)
2. open Aurora application on desktop of iMAC
3. select montage (1Person)
4. OPTIONAL: run calibration (triangle button on calibration screen)
5. click on diagram symbol, then start recording by clicking the circle
   |calibration|

EEG (Windows)
~~~~~~~~~~~~~

1. connect battery (grey box) to amp (other grey box) by placing
   underneath and plugging teal cord in (arrows up)
2. connect electrode set and ground to amp
3. open LSL streaming app on Windows machine (up to 3 instances)
4. enter the device serial number and click ‘link’, start with leopard,
   then lion, then tiger
5. if failed, unlink all devices from LSL, close all instances of LSL
   and repeat until all 3 devices are connected

after recording
---------------

**ALWAYS unplug the EEG batteries from each Amp and connect back to
their chargers**

.. |calibration| image:: https://github.com/val-pf/tomcat-equipment-wiki/blob/main/calibration2.jpg
