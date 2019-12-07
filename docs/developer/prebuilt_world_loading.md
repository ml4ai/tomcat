**Author:** Aditya Banerjee **Date:** 2019-12-7

# Pre-Built World Loading Tutorial

##  Loading Pre-Built Worlds

**Pre-requisites: Make sure you are able to run Minecraft and the default mission before attempting this tutorial.**

**Aim:** To load pre-built Minecraft worlds into the XML missions.

**Purpose:**  The pre-built world can have handmade structures which will help speed up mission development by shifting some load away from the XML. It is easier to build more complex environments by hand than through the XML. It will also allow us to set environment variables and command blocks in the world and control some aspects of the mission.


**Getting Started:**

The `sar`world is expected to be updated often, so the number attached to the name
will reflect the current version number.`x_x_x` is simply the placeholder to
represent this number. 

1\. Download the `sar_x_x_x` world from this Drive link: 

    https://drive.google.com/open?id=19PjgHmUeJmOQ4DS4dAy-4bgax2_TnJRp

Save it somewhere convenient and unzip the file. 

Henceforth, `sar_x_x_x` refers to the folder you got from unzipping.
	
**NOTE** : Ensure the path to which you save this folder has no white spaces.

Ex:  `Documents/folder name` is not allowed, but `Documents/folder_name` is allowed


2\. Download the `file_gen` and `file_gen_skeleton` XMLs from this Drive link:

    https://drive.google.com/drive/folders/1sA5F7ifgzglJnxFaeNuXHFwdMjmrpNHB?usp=sharing

`file_gen` will serve as a reference for you to use `file_gen_skeleton` to get your pre-built world working.

**DO NOT** save it inside  `sar_x_x_x`

This is because the _entire_ `sar_x_x_x`  folder represents the Minecraft world. It is not meant to hold anything else.

I saved mine in: 
`/home/$USER/Documents/TOMCAT/tomcat/external/malmo/sample_missions` to keep it with all the other missions


3\.	Open `file_gen_skeleton.xml`

Don’t change the name after we modify it (the next step) for the sake of this tutorial.


4\.	Modifying the XML

The comments in the XML indicate what needs to be filled in generally, but for
the tutorial go to the line `<FileWorldGenerator src = “” />`

Within the quotes specify the path to which you saved your `sar_x_x_x` folder.

Again,the **Minecraft world is the whole folder and not just one file inside the folder, so the path should end in:** `sar_x_x_x`
	
Refer to `file_gen.xml` to see how I specified my path. 

**It is important to note here that `~` and `$USER` won't work as placeholders inside the XML**

The whole path needs to be specified using `home/<your username>`


5\. For now, don’t modify any of the other variables. Simply save the XML.


6\. Launch Minecraft from the build directory in tomcat, and in a separate terminal from inside build
    
    run ./bin/runExperiment –mission <Path to the file_gen_skeleton.xml you modified>


7\.	That’s it! You should be inside the world I built.


##  Creating A World to Load

1\.	Simply launch Minecraft from inside the build folder using:

    ./launch_minecraft.sh


2\.	Single player →  Create new world


3\.	Name the world anything you want and hit create.

This next part of the step is optional. 

Select modify if you want it to be flat or use a specific seed for a certain world.
You can also set the game-play mode to survival or creative.
These modes are the  only options available during world creation, but spectator and adventure mode are available through in-game commands.


4\.	Just play the game!

Build monuments, buildings or anything you want.

You might even experiment with command blocks. For a list of commands and how to use them, refer to this link:

    https://minecraft.gamepedia.com/Commands


5\.	Once you’re done building, press Esc ->  Save and Quit.


6\. Navigate to `tomcat/external/malmo/Minecraft/run/saves/`


7\. Wrapping Up:

You should see the world you just played on as a folder in this directory.  You can now load this world. 

It is possible to load the world into the XML from this saves directory itself; however it is not recommended. 
Many worlds will be created each time a mission is run, and this directory will become hard to manage.
It is better to save it elsewhere and then load it in from that path.




## Final Notes

I intentionally left `file_gen_skeleton` as a very basic XML with no real mission elements. It is meant to serve as a foundation to build missions without needing to change or delete much.
When you’ve finished going through this documentation, you may add as much as you want to `file_gen_skeleton` to implement your mission. There is no need to follow
the tutorial's XML modification restrictions at that time.

Note however, it is better to set trivial paramaters through in-game commands while playing the world so that the XML can be more streamlined in its implementation of the mission itself. The goal of this endeavour is to move the trivial tasks of building and setting daytime etc. away from the XML.
