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

    /** Instance of the MqttService singleton to use for publishing messages. */
    private MqttService mqttService = MqttService.getInstance();

    /*
      Malmo uses an integrated server rather than a dedicated server, meaning
      that events will be fired twice and thus published twice if we are not
      careful.

      To overcome this, we will check the value of the isRemote attribute of the
      World whenever the World object is available from the event using the
      getWorld() method, and fall back to thread analysis using FMLCommonHandler
      when the world is not available.

      Note: I have chosen to publish the client-side events whenever possible,
      in the hopes that this will make the timestamps more accurate. - Adarsh
    */

    /**
     * Handle AttackEntityEvent events from Forge event bus, publish events to
     * MQTT message bus with type 'mob_attacked'.
     */
    @SubscribeEvent
    public void handle(AttackEntityEvent event) {
        // We use this technique to avoid double counting events due to the
        // integrated server nature of Malmo.
        if (this.fmlCommonHandler.getEffectiveSide() == Side.CLIENT) {
            Entity target = event.getTarget();
            if (target instanceof EntityMob) {
                this.mqttService.publish(new MobAttacked(event),
                                         "observations/events/mob_attacked");
            }
        }
    }

    /**
     * Handle events from Forge event bus triggered by players right-clicking a
     * block, publish events to MQTT message bus with type 'block_interaction'.
     */
    @SubscribeEvent
    public void handle(PlayerInteractEvent.RightClickBlock event) {
        if (!event.getWorld().isRemote) {
            this.mqttService.publish(new BlockInteraction(event),
                                     "observations/events/block_interaction");
        }
    }

    /**
     * Handle events from Forge event bus triggered by living entities dying
     * (players, mobs), publish events to MQTT message bus with type
     * 'entity_death'.
     */
    @SubscribeEvent
    public void handle(LivingDeathEvent event) {
        if (this.fmlCommonHandler.getEffectiveSide() == Side.CLIENT) {
            this.mqttService.publish(new EntityDeath(event),
                                     "observations/events/entity_death");
        }
    }
}
