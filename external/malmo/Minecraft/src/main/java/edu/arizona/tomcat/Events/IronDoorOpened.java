package edu.arizona.tomcat.Events;

import edu.arizona.tomcat.Events.Event;
import edu.arizona.tomcat.World.Position;
import net.minecraft.block.Block;
import net.minecraft.init.Blocks;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import net.minecraft.world.World;
import java.util.Date;
import java.text.DateFormat;
import java.text.SimpleDateFormat;

public class IronDoorOpened extends Event {
  private Position doorPosition;

  /** Constructor for use with the BlockAsistIron class. */
  public IronDoorOpened(BlockPos pos) {
    this.doorPosition = new Position(pos);
    this.eventType = "iron_door_opened";
  }
}
