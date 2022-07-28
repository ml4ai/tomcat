## Do these step if and only if Labrecorder isn't installed
1. Download **liblsl-1.14.0-focal_amd64** from https://github.com/sccn/liblsl/releases. After downloading you can double click on the liblsl-1.14.0-focal_amd64.deb file and install it via ubuntu software installer. 
2. Open a terminal, paste `sudo apt-get install qtbase5-dev` and hit enter. 
3. Download **LabRecorder-1.14.0-focal_amd64** from https://github.com/labstreaminglayer/App-LabRecorder/releases. After downloading you can double click on LabRecorder-1.14.0-focal_amd64.deb file and install it via ubuntu software installer. 
4. Open a terminal, paste `pip install pylsl` and hit enter. 

## Configure LabRecorder
1. Create a folder called **lsl_api** under Home directory. Then copy all the files in folder to lsl_api folder that you created. 
2. In order to execute LabRecorder, seperatly paste the following in three different terminals `./usr/LabRecorder/LabRecorde -c usr/cat/Home/LabRecorder_Leopard_22347.cfg`, `./usr/LabRecorder/LabRecorde -c LabRecorder_Lion_22345.cfg`, `./usr/LabRecorder/LabRecorde -c LabRecorder_Tiger_22346.cfg` and hit enter. 