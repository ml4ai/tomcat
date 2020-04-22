package edu.arizona.tomcat.Events;

import net.minecraft.world.World;
import edu.arizona.tomcat.Events.AttackEvent;
import edu.arizona.tomcat.Events.BlockEvent;
import net.minecraftforge.event.entity.player.AttackEntityEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import edu.arizona.tomcat.Messaging.MqttService;

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
      AttackEvent evt = new AttackEvent(event);
      this.mqttService.publish(evt, "observations/events");
  }

  @SubscribeEvent
  public void blockEvent(PlayerInteractEvent event) {
      BlockEvent evt = new BlockEvent(event);
      this.mqttService.publish(evt, "observations/events");
  }
}
