package edu.arizona.tomcat.Events;

import net.minecraft.world.World;
import edu.arizona.tomcat.Events.MobAttacked;
import edu.arizona.tomcat.Events.BlockInteraction;
import net.minecraft.util.math.BlockPos;
import net.minecraft.entity.Entity;
import net.minecraft.entity.monster.EntityMob;
import net.minecraftforge.event.entity.player.AttackEntityEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import edu.arizona.tomcat.Messaging.MqttService;

import com.google.gson.Gson;
import com.google.gson.FieldNamingPolicy;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;

public class ForgeEventHandler {

  // MQTT service
  private MqttService mqttService = MqttService.getInstance();
  /**
   * This event is triggered when the player attacks an enemy. It passes
   * specific event information to the helper method that prints the output.
   *
   * @param event - The event triggered. In this case the AttackEntityEvent
   */
  @SubscribeEvent
  public void attackEnemy(AttackEntityEvent event) {
      Entity target = event.getTarget();
      if (target instanceof EntityMob) {
        this.mqttService.publish(new MobAttacked(event), "observations/events/attack_mob");
      }
  }

  @SubscribeEvent
  public void blockEvent(PlayerInteractEvent.RightClickBlock event) {
      this.mqttService.publish(new BlockInteraction(event), "observations/events/block_interaction");
  }

}
