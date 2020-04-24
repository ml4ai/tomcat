package edu.arizona.tomcat.Events;

import edu.arizona.tomcat.World.Position;
import net.minecraft.util.math.BlockPos;

public class IronDoorOpened extends Event {
  private Position doorPosition;

  /** Constructor for use with the BlockAsistIron class. */
  public IronDoorOpened(BlockPos pos) {
    this.doorPosition = new Position(pos);
    this.eventType = "iron_door_opened";
  }
}
