package edu.arizona.tomcat.Events;

import net.minecraft.world.World;
import edu.arizona.tomcat.Events.MobAttacked;
import edu.arizona.tomcat.Events.BlockInteraction;
import edu.arizona.tomcat.Events.EntityDeath;
import net.minecraft.util.math.BlockPos;
import net.minecraft.entity.Entity;
import net.minecraft.entity.monster.EntityMob;
import net.minecraftforge.event.entity.player.AttackEntityEvent;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import net.minecraftforge.event.entity.living.LivingDeathEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import edu.arizona.tomcat.Messaging.MqttService;
import net.minecraftforge.fml.common.FMLCommonHandler;

import com.google.gson.Gson;
import com.google.gson.FieldNamingPolicy;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;

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
  @SideOnly(Side.CLIENT)
  @SubscribeEvent
  public void onEvent(AttackEntityEvent event) {
    // We use this technique to avoid double counting events due to the
    // integrated server nature of Malmo.
    if (this.fmlCommonHandler.getEffectiveSide() == Side.SERVER) {
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
    if (event.getWorld().isRemote) {
      this.mqttService.publish(new BlockInteraction(event), "observations/events/block_interaction");
    }
  }

  @SideOnly(Side.CLIENT)
  @SubscribeEvent
  public void onEvent(LivingDeathEvent event) {
    // We use this technique to avoid double counting events due to the
    // integrated server nature of Malmo.
    if (this.fmlCommonHandler.getEffectiveSide() == Side.SERVER) {
      this.mqttService.publish(new EntityDeath(event), "observations/events/entity_death");
    }
  }

}
