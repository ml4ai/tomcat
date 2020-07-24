# <center> Set-up instructions for Mumble Chat Client </center>


1. Download and launch:

	1. Zip file: [Mac] Following is an example for the latest version of Mumble:
` wget https://dl.mumble.info/stable/Mumble-1.3.2.dmg`
`unzip Mumble-1.3.2.dmg`<br> 
`hdiutil attach Mumble-1.3.2.dmg` [Mount DMG]<br> 
`open –a /Volumes/Mumble\ 1.3.2/`[Open and launch app]
	 
	1. 	 Linux: PPA (Personal Package Archive) provided:
	PPA's location: `ppa:gwibber-daily/ppa`
	`sudo add-apt-repository ppa:user/ppa-name`
	`sudo apt-get update` <br>
[Replace ppa:user/ppa-name with the PPA's location that you noted above.]
From the Mumbel website: "Your system will now fetch the PPA's key. This enables your Ubuntu system to verify that the packages in the PPA have not been interfered with since they were built."
"Now, as a one-off, you should tell your system to pull down the latest list of software from each archive it knows about, including the PPA you just added:

		For Mumble: `sudo add-apt-repository ppa:mumble/release`
	`sudo apt-get update`
	
1. 	 SSL certificate: follow instructions for 'certificate wizard' and and allow app to store the newly createdcertificate.
2. Audio Wizard: 
	3. 	Introduction, device selection: click next
	4. Volume tuning: choose lowest value(default 10ms, lowest)
	5. Voice Activity detection: Speech in yellow and green bar and choose 'raw amplitude from input'
	6. Quality and notifications: custom, and disable Text-to-speech
7. Connecting to server: click on the globe symbol, then 'add new': enter details 
8. Record: click red record symbol, then:
	9. 	select directory for storing output, format
	10. mode: 'multichannel' for 1 file per speaker
	11. Filename:`%user%date%time` will store relevant information. Add more text to start or finish filename
12. Settings: set up audio input and output [in case more fine-tuned audio detection is needed]
	 