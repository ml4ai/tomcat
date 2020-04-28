package edu.arizona.tomcat.Events;

import edu.arizona.tomcat.Messaging.MqttService;
import net.minecraft.block.Block;
import net.minecraft.block.BlockDoor;
import net.minecraft.block.BlockLever;
import net.minecraft.entity.Entity;
import net.minecraft.entity.monster.EntityMob;
import net.minecraft.util.EnumHand;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.entity.living.LivingDeathEvent;
import net.minecraftforge.event.entity.player.AttackEntityEvent;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import net.minecraftforge.event.world.BlockEvent;
import net.minecraftforge.fml.common.FMLCommonHandler;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.fml.relauncher.Side;

public class ForgeEventHandler {

    private FMLCommonHandler fmlCommonHandler = FMLCommonHandler.instance();

    private Block getBlock(PlayerInteractEvent.RightClickBlock event) {
        return event.getWorld().getBlockState(event.getPos()).getBlock();
    }

    private static ForgeEventHandler instance = null;
    private ForgeEventHandler() { MinecraftForge.EVENT_BUS.register(this); }
    public static ForgeEventHandler getInstance() {
        if (instance == null) {
            instance = new ForgeEventHandler();
        }
        return instance;
    }

    /** Instance of the MqttService singleton to use for publishing messages. */
    private MqttService mqttService = MqttService.getInstance();

    /* Malmo uses an integrated server rather than a dedicated server, meaning
     * that events will be fired twice and thus published twice if we are not
     * careful.
     *
     * To overcome this, we will use the getSide() method of the event if it is
     * available, and fall back to thread analysis using FMLCommonHandler when
     * it is not.
     *
     * Note: I have chosen to publish the client-side events whenever possible,
     * in the hopes that this will make the timestamps more accurate. - Adarsh
     * */

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
     * Handle EntityInteract events from Forge event bus.
     */
    @SubscribeEvent
    public void handle(PlayerInteractEvent.EntityInteract event) {
        // We use this technique to avoid double counting events due to the
        // integrated server nature of Malmo.
        if (event.getSide() == Side.CLIENT) {
            this.mqttService.publish(
                new EntityInteraction(event),
                "observations/events/player_interactions/entity_interactions");
        }
    }

    /**
     * Handle events from Forge event bus triggered by players right-clicking a
     * block, publish events to MQTT message bus.
     */
    @SubscribeEvent
    public void handle(PlayerInteractEvent.RightClickBlock event) {
        // RightClickBlock events that don't produce a result (i.e.
        // right-clicking things that are not doors, levers, etc.) result in
        // two events firing per right click - one for each hand. To avoid
        // duplication, we only publish the events corresponding to the main
        // hand.
        if (event.getSide() == Side.CLIENT &&
            event.getHand() == EnumHand.MAIN_HAND) {
            Block block = this.getBlock(event);
            if (block instanceof BlockLever) {
                this.mqttService.publish(
                    new LeverFlip(event),
                    "observations/events/player_interactions/blocks/lever");
            }
            else if (block instanceof BlockDoor) {
                this.mqttService.publish(
                    new DoorInteraction(event),
                    "observations/events/player_interactions/blocks/door");
            }

            else {
                this.mqttService.publish(
                    new BlockInteraction(event),
                    "observations/events/player_interactions/blocks");
            }
        }
    }

    /**
     * Handle events from Forge event bus triggered by players left-clicking a
     * block, publish events to MQTT message bus.
     */
    @SubscribeEvent
    public void handle(PlayerInteractEvent.LeftClickBlock event) {
        if (event.getSide() == Side.CLIENT &&
            event.getHand() == EnumHand.MAIN_HAND) {
            this.mqttService.publish(
                new BlockInteraction(event),
                "observations/events/player_interactions/left_clicks/blocks");
        }
    }

    /**
     * Handle events from Forge event bus triggered by players breaking blocks.
     */
    @SubscribeEvent
    public void handle(BlockEvent.BreakEvent event) {
        if (!event.getWorld().isRemote) {
            this.mqttService.publish(
                new BlockBreakEvent(event),
                "observations/events/player_interactions/break_events/blocks");
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
