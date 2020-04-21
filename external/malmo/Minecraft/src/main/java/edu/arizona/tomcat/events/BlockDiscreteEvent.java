package edu.arizona.tomcat.events;

import edu.arizona.tomcat.events.Event;
import net.minecraft.block.Block;
import net.minecraft.init.Blocks;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import net.minecraft.world.World;
import java.util.Date;
import java.text.DateFormat;
import java.text.SimpleDateFormat;

public class BlockDiscreteEvent extends Event {
  private String eventName;
  private String timestamp;
  private String coordinates;

  public BlockDiscreteEvent(PlayerInteractEvent event) {
    if (event.getClass() == PlayerInteractEvent.RightClickBlock.class) {
      World world = event.getWorld();
      BlockPos pos = event.getPos();
      Block block = world.getBlockState(pos).getBlock();
      EntityPlayer playerIn = event.getEntityPlayer();
      if (block.equals(Blocks.STONE_BUTTON)) {
        eventName = "button_pressed";
      }
      else if (block.equals(Blocks.LEVER)) {
        eventName = "lever_flipped";
      }
      DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
      Date date = new Date();

      String timestamp = dateFormat.format(date); // Date and Time
      int x = pos.getX(), y = pos.getY(), z = pos.getZ(); // event coordinates
      String coordinates = "X: " + x + " "
                          + "Y: " + y + " "
                          + "Z: " + z;

      String playerName = "";
      if (playerIn != null) {
        playerName = playerIn.getDisplayNameString();
      }

      this.eventName = eventName;
      this.timestamp = timestamp;
      this.coordinates = coordinates;
    }
  }
}
