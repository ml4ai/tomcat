**Author:** Aditya Banerjee                                                 
**Date:** 2019-11-24

# Notes for The Mission Worlds

Before reading through the notes, it might be helpful to familiarize yourself
with Minecraft's in-game command system at:

    https://minecraft.gamepedia.com/Commands

### Search and Rescure Mission World

This is the world used for the Search and Rescue(SAR) Mission, and it can
be downloaded from this Drive link as the `SAR_v1` folder:

    https://drive.google.com/open?id=19PjgHmUeJmOQ4DS4dAy-4bgax2_TnJRp 

Before loading the world, it is important to note that some environment parameters have been modified in-game at the time the world was built. These are:

1\. The day-night cycle has been disabled with the `/gamerule doDaylightCycle`
command. To re-enable day-night cycles, use the following command in-game:

    /gamerule doDaylightCycle true

2\. The weather cycle has been disabled with the `/gamerule doWeatherCycle`
command. To re-enable weather cycles, use the following command in-game:
    
    /gamerule doWeatherCycle true

**Note:** The day-night and weather cycles have been turned on to keep the
environment constant. The `/time set` and `/weather`commands can still be used
to change the time and weather. The environment variables above only control
that it won't change on its own after that.

Once the world has been downloaded and loaded into Minecraft, the player should spawn at the coordinates I set in-game using the `/setworldspawn` command. In case this doesn't work, however, `<Placement/>` in the  XML can be modified with `x = 22 y = 64 z = 73 yaw = "-90"` to fix the issue.

**Implementing A Mission:**

The world consists of a region closed off by stone walls that is meant to serve
as the mission space. Inside the mission space there are structures meant to
house the mobs the player needs to kill or rescue. The coordinates for these
structures can be found using the reference image at:

    https://drive.google.com/open?id=1sA5F7ifgzglJnxFaeNuXHFwdMjmrpNHB

Spawning entities at the coordinates in the reference image will spawn that
entity _inside_ the structure. 

It should also be noted that all the doors used in SAR_v1 world are iron doors.
Wooden doors wern't used in order to prevent mobs from randomly opening doors
by themselves. 

The doors can, however, be opened by a player who can
right-click the lever next to the door and open it. This is true for all the
doors in the mission except the "final door" in a structure. This final door is
meant to be opened programatically once the player has successfully
killed/saved (depends on the mob) the mob in the chamber.

**NOTE:** Some of the smaller structures have no "final doors".

