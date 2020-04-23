package edu.arizona.tomcat.Events;

import edu.arizona.tomcat.Messaging.MqttService;
import net.minecraft.entity.Entity;
import net.minecraft.entity.monster.EntityMob;
import net.minecraftforge.event.entity.living.LivingDeathEvent;
import net.minecraftforge.event.entity.player.AttackEntityEvent;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import net.minecraftforge.fml.common.FMLCommonHandler;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

public class ForgeEventHandler {

  private FMLCommonHandler fmlCommonHandler = FMLCommonHandler.instance();
  // MQTT service
  private MqttService mqttService = MqttService.getInstance();
  /**
   * This event is triggered when the player attacks an enemy. It passes
   * specific event information to the helper method that prints the output.
   *
   * @param event - The event triggered. In this case the AttackEntityEvent
   */
  @SubscribeEvent
  public void onEvent(AttackEntityEvent event) {
    // We use this technique to avoid double counting events due to the
    // integrated server nature of Malmo.
    if (this.fmlCommonHandler.getEffectiveSide() == Side.CLIENT) {
      Entity target = event.getTarget();
      if (target instanceof EntityMob) {
        this.mqttService.publish(new MobAttacked(event), "observations/events/mob_attacked");
      }
    }
  }

  @SubscribeEvent
  public void onEvent(PlayerInteractEvent.RightClickBlock event) {
    // We use this technique (event.getWorld().isRemote() to avoid double
    // counting events due to the integrated server nature of Malmo.
    if (!event.getWorld().isRemote) {
      this.mqttService.publish(new BlockInteraction(event), "observations/events/block_interaction");
    }
  }

  @SubscribeEvent
  public void onEvent(LivingDeathEvent event) {
    // We use this technique to avoid double counting events due to the
    // integrated server nature of Malmo.
    if (this.fmlCommonHandler.getEffectiveSide() == Side.CLIENT) {
      this.mqttService.publish(new EntityDeath(event), "observations/events/entity_death");
    }
  }

}
