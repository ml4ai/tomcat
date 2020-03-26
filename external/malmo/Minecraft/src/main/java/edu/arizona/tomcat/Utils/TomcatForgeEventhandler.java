package edu.arizona.tomcat.Utils;

import net.minecraft.entity.Entity;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.event.entity.player.AttackEntityEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;

public class TomcatForgeEventhandler {

  /**
   * This event is triggered when the player attacks an enemy. It passes
   * specific event information to the helper method that prints the output.
   *
   * @param event - The event triggered. In this case the AttackEntityEvent
   */
  @SubscribeEvent
  public void attackEnemy(AttackEntityEvent event) {
    EntityPlayer playerIn = event.getEntityPlayer();
    Entity enemy = event.getTarget();

    BlockPos pos = new BlockPos(
        event.getTarget().posX,
        event.getTarget().posY,
        event.getTarget().posZ); // Event occurrence is location of target

    DiscreteEventsHelper.printAttackEventOccurence(pos, enemy, playerIn);
  }
}
