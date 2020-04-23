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

public class BlockEvent extends Event {
  private String eventType = "block";
  private String playerName = null;
  private Position blockPosition;
  private String blockType = "";

  /** A constructor for general block interaction events. */
  public BlockEvent(PlayerInteractEvent event) {
    World world = event.getWorld();
    BlockPos pos = event.getPos();
    this.playerName = event.getEntityPlayer().getDisplayNameString();
    this.blockPosition = new Position(pos);
    this.blockType = world.getBlockState(pos).getBlock().getClass().getName();
  }

  /** Secondary constructor, for use with the BlockAsistIron class. */
  public BlockEvent(BlockPos pos, String eventName) {
    this.blockPosition = new Position(pos);
    this.eventType = "asist_iron_door_opened";
  }
}
