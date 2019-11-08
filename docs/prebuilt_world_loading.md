Loading pre-built Worlds.
=========================

Aim: To load pre-built Minecraft worlds into the XML missions. 

Purpose: The pre-built world can have structures and command blocks that define and control various aspects of the mission which we won’t need to code in XML anymore.

Getting Started:

    1. Download the Tomcat_Preloaded world from this google drive link: 

https://drive.google.com/drive/folders/1sA5F7ifgzglJnxFaeNuXHFwdMjmrpNHB?usp=sharing

	and save it somewhere convenient. Unzip the file. Henceforth Tomcat_Preloaded refers
	to the folder that you got from unzipping.
	**NOTE: Ensure the path to where you save this folder has no white spaces.
	Ex:  Documents/folder name is not allowed but Documents/folder_name is allowed**

    2. Download the file_gen and file_gen_skeleton XML s from this google drive link:

https://drive.google.com/drive/folders/1sA5F7ifgzglJnxFaeNuXHFwdMjmrpNHB?usp=sharing

	file_gen will serve as a reference for you to use file_gen_skeleton to get your pre-built world 	working. DO NOT SAVE IT INSIDE Tomcat_Preloaded
	 I saved mine in: 
	/home/adi-ua/Documents/TOMCAT/tomcat/external/malmo/sample_missions

3.	Open file_gen_skeleton.xml. Don’t change the name after we modify it for the sake of the 	tutorial

4.	The comments in the XML indicate what needs to be filled in generally, but for the tutorial go to 	the line <FileWorldGenerator src = “” />

	Within the quotes specify the path to which you saved your Tomcat_Preloaded folder download.
	The Minecraft world is the whole folder and not just one file inside the folder, so the path 	should end in Tomcat_Preloaded
	
	Refer to file_gen.xml to see how I specified my path. 

	It is important to note here that ~ and 	$USER wont work as placeholders and the 	whole path needs to be specified using home/ <	your 	username>

5.  For now, don’t modify any of the other variables. Simply save the XML.

6.  Launch Minecraft from the build directory in tomcat and in a separate terminal from inside build run ./bin/runExperiment –mission <Path to the file_gen_skeleton.xml you modified>

7. 	That’s it! You should be inside the world I built.
	Press Enter to start playing. 
	Open the chest I placed with right click, and read the book titled READ_ME that I placed 	inside with some basic instructions.


Additional Notes for when you're done with the tutorial:

I’ve placed a few command blocks in-game with the following commands:

Note: This part of the instruction is mostly repeated in my READ_ME in-game. You can activate the commands by right clicking the button on the command block. You can also access the command itself by right clicking the command block.

1.  set time day: It sets the time to daytime (This is probably a good command that you might set to always run in the background if you want to maintain daytime. More information on that below)

2.  give @p command_block: This is from the family of give @p [object] commands where an item can be given to someone or something. Here, @p simply means that the item is given to the player closest to the place where the command is executed.
Other than @p, @a and @e also exist.
@a targets all players in the world. So tp @a <x> <y> <z> teleport everyone to a certain place
@e targets all entities including mobs. So the command kill @e would kill everything including you.
Specifying a player’s name instead of the @ modifier targets the specific player. This is especially useful when there’s multiple players in a single world
There’re more modifiers not listed here.

3. summon Zombie ~ ~ ~:  Spawns the specified mob to the location of the calling function. The ~ ~ ~ means that the Zombie is summoned at the place of the calling function. In this case it spawns at the location of the command block. If however, the player enters the command as /summon Zombie ~ ~ ~ in their chat, the Zombie is summoned right at their location. The ~ ~ ~ can be replaced with <x> <y> <z> to spawn it at a specific place.

4. gamerule keepInventory true: Makes the environment such that if the player dies then they don’t lose the stuff they had in their inventory like they would usually. This comes from the environment modifying family of commands. There’s quite a few, so I’ve not covered them all here, but these can be used to stop explosions from breaking things when they explode, disable command feedback, modify ticks and so on.

[The functions I used here are simple but composite commands can be created. I’ll upload and demonstrate a sample of such a composite command soon.]

Here’s some documentation for more extensive information on what’s possible:
 https://minecraft.gamepedia.com/Commands


Making your own pre-built world:

1. 	Simply launch Minecraft from inside the build folder

2. 	Single player → Create new world
3. 	Name the world anything you want and select modify if you want it to be flat or use a specific 	seed and game-play mode(survival creative are the only options available during world creation 	but spectator and adventure mode are available through in-game commands)
4.	Just play the game. Build structures. Experiment with command blocks.
5. 	Once you’re done building. Press Esc and Save and quit.
6. 	Navigate to tomcat/external/malmo/Minecraft/run/saves/
7.  	You should see the world you just played on as a folder in this directory.  You can now preload 	this world. It is possible to load it from this directory itself similar to how you loaded 	Tomcat_Preloaded in the tutorial, but it is not recommended. Many worlds will be create each 	time a mission is run and this directory becomes hard to manage, so it is better to save it 	elsewhere and then load it in from that path.






I intentionally left file_gen_skeleton as a very basic XML with no real mission elements. It is meant to serve as a foundation to build missions without needing to change or delete much. I will be making some more fleshed out missions where I will use the skeleton XML as my base.  When you’ve finished going through this documentation, you may add as much as you want to the file_gen_skeleton.xml to implement your mission. There is no need to follow the tutorials XML modification restrictions at that time. However, since the point of this is to move some parts of the mission to in-game command blocks, I recommend using command blocks to implement commands in-game and simply load that world in.



 


