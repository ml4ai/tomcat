package edu.arizona.tomcat.Events;

import com.microsoft.Malmo.MalmoMod;
import edu.arizona.tomcat.Messaging.MqttService;
import edu.arizona.tomcat.Mission.Mission;
import edu.arizona.tomcat.Mission.ZombieMission;
import edu.arizona.tomcat.Utils.MinecraftServerHelper;
import net.minecraft.block.Block;
import net.minecraft.block.BlockDoor;
import net.minecraft.block.BlockLever;
import net.minecraft.entity.Entity;
import net.minecraft.entity.monster.EntityMob;
import net.minecraft.entity.passive.EntityVillager;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.util.EnumHand;
import net.minecraft.world.World;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.CommandEvent;
import net.minecraftforge.event.ServerChatEvent;
import net.minecraftforge.event.entity.living.LivingDeathEvent;
import net.minecraftforge.event.entity.living.LivingEquipmentChangeEvent;
import net.minecraftforge.event.entity.player.AttackEntityEvent;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;
import net.minecraftforge.event.world.BlockEvent;
import net.minecraftforge.fml.common.gameevent.PlayerEvent.ItemPickupEvent;
import net.minecraftforge.fml.common.FMLCommonHandler;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.event.entity.player.EntityItemPickupEvent;
import net.minecraftforge.fml.relauncher.Side;
import java.nio.file.Paths;
import java.io.*;
import java.nio.file.Files;
import java.util.Map;
import com.google.gson.Gson;

public class ForgeEventHandler {

    private FMLCommonHandler fmlCommonHandler = FMLCommonHandler.instance();
    private int zombieMissionVillagersSaved = 0;

    private Block getBlock(PlayerInteractEvent.RightClickBlock event) {
        return event.getWorld().getBlockState(event.getPos()).getBlock();
    }

    private static ForgeEventHandler instance = null;
    private ForgeEventHandler() {}

    public static ForgeEventHandler getInstance() {
        if (instance == null) {
            instance = new ForgeEventHandler();
            MinecraftForge.EVENT_BUS.register(instance);
        }
        return instance;
    }

    /**
     * Unregister as a listener to Minecraft events
     */
    public static void unregister() {
        MinecraftForge.EVENT_BUS.unregister(instance);
        instance = null;
    }

    /**
     * This method checks for extra events at every tick.
     */
    public void updateExtraEvents() {
        Mission mission =
            MalmoMod.instance.getServer().getTomcatServerMission();
        if (mission != null) {
            this.checkVillagerSavedEvent(mission);
        }
    }


    /**
     * This method reads the devmode json config and returns whether devmode
     * should be enabled or disabled for CommandEvent handling purposes
     */
    public static boolean readDevModeFromJson() {
        Gson gson = new Gson();
        Reader reader = null;

        try {
            reader = Files.newBufferedReader(Paths.get("devmode.json"));
            Map<String, Boolean> devmode = gson.fromJson(reader, Map.class);
            reader.close();
            System.out.println("Devmode status read as: " + devmode.get("devmode").toString());
            return devmode.get("devmode");
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        System.out.println("Devmode read failed; default to false.");
        return false;
    }
    
    /**
     * Called by checkExtraEvents at every tick to see if a villager has been
     * saved in the given mission
     *
     * @param mission - The current mission
     */
    private void checkVillagerSavedEvent(Mission mission) {
        if (mission instanceof ZombieMission) {

            ZombieMission zombieMission = (ZombieMission)mission;
            // If the number of villagers saved has gone up, then a villager was
            // saved. Checking this avoids having to check the loadedEntityList
            // unnecessarily
            boolean villagerSaved = zombieMission.getNumberOfVillagersSaved() -
                                        this.zombieMissionVillagersSaved >
                                    0;

            if (villagerSaved) {
                this.zombieMissionVillagersSaved += 1;
                World world =
                    MinecraftServerHelper.getServer().getEntityWorld();

                for (Entity entity : world.getLoadedEntityList()) {
                    if (entity instanceof EntityVillager) {
                        for (EntityPlayerMP player :
                             MinecraftServerHelper.getServer()
                                 .getPlayerList()
                                 .getPlayers()) {
                            if (player.getDistanceToEntity(entity) <=
                                ZombieMission.MAX_DISTANCE_TO_SAVE_VILLAGER) {
                                this.mqttService.publish(
                                    new VillagerSaved(entity),
                                    "observations/events/player_interactions/villager_saved");
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Instance of the MqttService singleton to use for publishing messages.
     */
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
                    "observations/events/player_interactions/blocks/general");
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
                "observations/events/player_interactions/blocks/general");
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
                "observations/events/player_interactions/blocks/break");
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

    /**
     * Handle events from Forge event bus triggered by chat messages.
     */
    @SubscribeEvent
    public void handle(ServerChatEvent event) {
        this.mqttService.publish(new Chat(event), "observations/chat");
    }

    /** Command event handler
     * 
     *  Currently, just handles turning dev mode on/off; all commands are
     *  enabled in dev mode and all are disabled out of dev mode.
     */
    @SubscribeEvent
    public void handle(CommandEvent event) {
        if (!readDevModeFromJson()) {
            // whitelisting events would be done here
            if (event.isCancelable()) {
                event.setCanceled(true);
            }
        }
    }

    /** EntityItemPickupEvent handler */
    @SubscribeEvent
    public void handle(EntityItemPickupEvent event) {
        this.mqttService.publish(new ItemPickedUp(event), "observations/events/player_interactions/item_pickup");
    }

    /** LivingEquipmentChange handler */
    @SubscribeEvent
    public void handle(LivingEquipmentChangeEvent event) {
        this.mqttService.publish(new EquipmentChange(event), "observations/events/player_interactions/equipment_change");
    }
}
