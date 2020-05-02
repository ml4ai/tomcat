package edu.arizona.tomcat.Events;

import edu.arizona.tomcat.World.Position;
import net.minecraft.util.math.BlockPos;

public class HitControlledDoorOpened extends Event {
    private Position doorPosition;

    /** Constructor for use with the BlockAsistIron class. */
    public HitControlledDoorOpened(BlockPos pos) {
        this.doorPosition = new Position(pos);
    }
}
