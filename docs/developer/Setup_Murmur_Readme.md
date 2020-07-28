# Setting up the Murmer server for Unix systems:

1.  Obtain a DNS domain and create one with a dynamic dns provider (e.g. noip.com). Make IP static if computers outside the network need to connect.
1. Enable port forwarding for port number 64738
1. Port forward TCP, UDP port 64738 
2. Downloading Murmer:<br> 
	[May require superuser privileges for the setup to work]
	 ` wget https://dl.mumble.info/stable/murmur-static_x86-1.3.2.tar.bz2`
	`tar -zxvf murmur-static_x86-1.3.2.tar.bz2`
1.  Open with `vim murmur.ini` to make changes: <br>
	To make the server private, change line 	`serverpassword=` with a pass phrase. [Can be skipped].
3. Run with `./murmurd -fg -ini murmur.ini` <br>
 [Note down the username and password that is listed during the first launch].  
1. In version >=1.2.4 the SuperUser password is generated automatically on the first server start. You can find it in the logfile. Search for an entry like 	`<W>2013-09-03 11:23:44.516 1 => Password for 'SuperUser' set to 'supersecretpassword'` 
This can be changed <br>
[This is all that was required on a Mac laptop for server setup]

***Some additional steps that may be required: <br>***
To set/change the password on the Linux static server, run <br> 
	`murmur.x86 -ini <path to configuration file> -supw <password> [srv]`	
To set the password on Debian-based systems, run EITHER use the dpkg management facilities <br>
 	`sudo dpkg-reconfigure mumble-server` <br>
 OR run the server binary manually: <br>
 	`sudo -i murmurd -ini /etc/mumble-server.ini -supw <password> [srv]`
 
