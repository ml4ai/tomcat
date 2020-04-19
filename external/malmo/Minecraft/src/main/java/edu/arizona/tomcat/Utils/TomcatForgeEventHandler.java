package edu.arizona.tomcat.Utils;

import net.minecraft.block.Block;
import net.minecraft.entity.Entity;
import net.minecraft.entity.monster.EntityMob;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.init.Blocks;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.event.entity.player.AttackEntityEvent;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
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

  @SubscribeEvent
  public void blockEvent(PlayerInteractEvent event) {
    if (event.getClass() == PlayerInteractEvent.RightClickBlock.class) {
      World world = event.getWorld();
      BlockPos pos = event.getPos();
      Block block = world.getBlockState(pos).getBlock();
      EntityPlayer playerIn = event.getEntityPlayer();
      if (block.equals(Blocks.STONE_BUTTON)) {

        DiscreteEventsHelper.writeBlockEvent(pos, playerIn, "button_pressed");
      }
      else if (block.equals(Blocks.LEVER)) {
        DiscreteEventsHelper.writeBlockEvent(pos, playerIn, "lever_flipped");
      }
    }
  }
}
