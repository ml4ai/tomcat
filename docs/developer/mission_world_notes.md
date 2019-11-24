**Author:** Aditya Banerjee **Date:** 2019-11-24

# Notes for The Mission Worlds

Before reading through the notes, it might be helpful to familiarize yourself
with Minecraft's in-game command system at:

    https://minecraft.gamepedia.com/Commands


## Tutorial Mission World

This is the world used for the Tutorial Mission, and it can
be downloaded from this Drive link as the `Tutorial_v0.01` folder:

    https://drive.google.com/open?id=19PjgHmUeJmOQ4DS4dAy-4bgax2_TnJRp 

The environment parameters modified in-game at the time the world was built were as follows:

1\. The day-night cycle was disabled with the `/gamerule doDaylightCycle`
command. To re-enable day-night cycles, use the following command in-game:

    /gamerule doDaylightCycle true

2\. The weather cycle was disabled with the `/gamerule doWeatherCycle`
command. To re-enable weather cycles, use the following command in-game:
    
    /gamerule doWeatherCycle true

3\. Mob spawning was disabled with the `/gamerule doMobSpawning` command.To
re-enable mob spawning, use the following command in-game:

    /gamerule doMobSpawning true

4\. The world's default spawnpoint for all players was set to `x = -623 y = 4 z
= 1584` using the `/setworldspawn` command. To modify the spawnpoint, use new
coordinates in the  `<x> <y> <z>` spaces (without <>) in the following command:

    /setworldspawn <x> <y> <z>


## Search and Rescure Mission World

This is the world used for the Search and Rescue(SAR) Mission, and it can
be downloaded from this Drive link as the `SAR_v1` folder:

    https://drive.google.com/open?id=19PjgHmUeJmOQ4DS4dAy-4bgax2_TnJRp 

The environment parameters modified in-game at the time the world was built were as follows:

1\. The day-night cycle was disabled with the `/gamerule doDaylightCycle`
command. To re-enable day-night cycles, use the following command in-game:

    /gamerule doDaylightCycle true

2\. The weather cycle was  disabled with the `/gamerule doWeatherCycle`
command. To re-enable weather cycles, use the following command in-game:
    
    /gamerule doWeatherCycle true

3\. The world's default spawnpoint for all players was set to `x = 22 y = 64 z
= 73` using the `/setworldspawn` command. To modify the spawnpoint, use new
coordinates in the  `<x> <y> <z>` spaces (without <>) in the following command:

    /setworldspawn <x> <y> <z>


It should be noted that the doors used in `SAR_v1` world are all iron doors.
Wooden doors wern't used in order to prevent mobs from randomly opening doors
by themselves (this could be a non-issue depending on the mob you are using). The doors can, however, be opened by a player who can right-click the lever next to the door and open it. This is true for all the doors in the mission except the "final door" in a structure. This final door is meant to be opened programatically once the player has successfully killed/saved (depends on the mob) the mob in the chamber.

**NOTE:** Some of the smaller structures have no "final doors".

## Common  Mission Implementation Notes

The day-night and weather cycles were turned off to keep the
environment constant. The `/time set` and `/weather`commands can still be used
to change the time and weather. The environment variables (the modified
world parameters) above only control that it won't change on its own after that.

Similarly, mob spawning was turned off to prevent creatures from spawning
randomly. Mobs can still be spawned using the `/summon` command.

Reference images for noteworthy points on the worlds can be found at:

    https://drive.google.com/open?id=1sA5F7ifgzglJnxFaeNuXHFwdMjmrpNHB

Spawning entities at the coordinates in the reference image will spawn that
entity _inside_ the structure, not on top of it.

**A Potential Issue**

Once the world has been downloaded and loaded into Minecraft, the player should spawn at the coordinates I set in-game using the `/setworldspawn` command. In case this doesn't work, however, `<Placement/>` in the  XML can be modified with `x = "" y = ""  z = "" yaw = "" ` (whatever coordinates you need for that world) to fix the issue.
