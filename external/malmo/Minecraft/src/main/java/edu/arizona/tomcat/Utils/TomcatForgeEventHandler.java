package edu.arizona.tomcat.Utils;

import net.minecraft.entity.Entity;
import net.minecraft.entity.monster.EntityMob;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.event.entity.player.AttackEntityEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;

public class TomcatForgeEventHandler {

  /**
   * This event is triggered when the player attacks an enemy. It passes
   * specific event information to the helper method that prints the output.
   *
   * @param event - The event triggered. In this case the AttackEntityEvent
   */
  @SubscribeEvent
  public void attackEnemy(AttackEntityEvent event) {
    EntityPlayer playerIn = event.getEntityPlayer();
    Entity target = event.getTarget();

    if (target instanceof EntityMob) {

      EntityMob enemy = (EntityMob)target;

      BlockPos pos = new BlockPos(
          event.getTarget().posX,
          event.getTarget().posY,
          event.getTarget().posZ); // Event occurrence is location of target

      DiscreteEventsHelper.writeAttackEvent(pos, enemy, playerIn);
    }
  }
}
