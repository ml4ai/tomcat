package edu.arizona.tomcat.Mission;

import com.microsoft.Malmo.Schemas.BlockType;
import com.microsoft.Malmo.Schemas.EntityTypes;
import com.microsoft.Malmo.Schemas.ItemType;
import com.microsoft.Malmo.Schemas.PosAndDirection;
import edu.arizona.tomcat.Messaging.TomcatClientServerHandler;
import edu.arizona.tomcat.Messaging.TomcatMessageData;
import edu.arizona.tomcat.Messaging.TomcatMessaging;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessage;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessageType;
import edu.arizona.tomcat.Mission.Goal.ActivateButtonGoal;
import edu.arizona.tomcat.Mission.Goal.ApproachEntityGoal;
import edu.arizona.tomcat.Mission.Goal.KillEntityGoal;
import edu.arizona.tomcat.Mission.Goal.MissionGoal;
import edu.arizona.tomcat.Mission.Goal.ReachPositionGoal;
import edu.arizona.tomcat.Mission.Goal.StartGoal;
import edu.arizona.tomcat.Mission.MissionPhase.CompletionStrategy;
import edu.arizona.tomcat.Mission.gui.RichContent;
import edu.arizona.tomcat.Mission.gui.SelfReportContent;
import edu.arizona.tomcat.Utils.InventoryHandler;
import edu.arizona.tomcat.Utils.MinecraftServerHelper;
import edu.arizona.tomcat.World.Drawing;
import edu.arizona.tomcat.World.TomcatEntity;
import java.math.BigDecimal;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public class TutorialMission extends Mission {

    private static final int MAX_DISTANCE_TO_SAVE_VILLAGER = 1;
    private static final float SECONDS_PER_CAMERA_VIEW = 1.5f;

    private double viewTime;
    private boolean shouldSpawnSkeletonInTheArena;
    private boolean shouldSpawnZombieInsideTheBuilding;
    private boolean shouldSpawnVillagerInsideTheBuilding;
    private boolean shouldaddMaterialsToUsersInventory;
    private boolean shouldSpawnExposedEntities;
    private MissionPhase addInstructionsPhase;
    private MissionPhase approachPoolsPhase;
    private MissionPhase enterTheArenaPhase;
    private MissionPhase killSkeletonPhase;
    private MissionPhase killZombiePhase;
    private UUID skeletonUUID;
    private UUID zombieUUID;
    private UUID villagerUUID;
    // This variable has to be static because objects are inserted in it in a
    // method called by a Minecraft event and updating an object variable does
    // not work.
    private static Set<EntityPlayer> deadPlayers = new HashSet<EntityPlayer>();

    public static final int NUMBER_OF_VILLAGERS = 1;

    public TutorialMission() {
        super();
        this.id = ID.TUTORIAL;
        this.viewTime = 0;
        this.shouldSpawnSkeletonInTheArena = false;
        this.shouldSpawnZombieInsideTheBuilding = false;
        this.shouldSpawnVillagerInsideTheBuilding = false;
        this.shouldSpawnExposedEntities = false;
        this.skeletonUUID = UUID.randomUUID();
        this.zombieUUID = UUID.randomUUID();
        this.villagerUUID = UUID.randomUUID();
    }

    @Override
    public void init(World world) {
        super.init(world);
    }

    @Override
    protected void createPhases() {
        this.addInstructionsPhase();
        this.addApproachPoolsPhase();
        /*
         * TODO - The crafting phase is going to be commented out until we have
         * crafting in our main mission. So far, there's no crafting so there's
         * no need to present this in the tutorial
         */
        // this.addCraftItemPhase();
        this.addApproachEntitiesPhase();
        this.addEnterTheArenaPhase();
        this.addKillSkeletonPhase();
        this.addKillZombiePhase();
        this.addSaveVillagerPhase();
        this.addLeaveTheBuildingPhase();
    }

    /**
     * When the tutorial first pops up,
     * it would be helpful to have an orienting screen with general
     * instructions.
     */
    private void addInstructionsPhase() {
        RichContent instructions =
            RichContent.createFromJson("tutorial_orienting_screen.json");
        this.addInstructionsPhase =
            new MissionPhase(instructions,
                             CompletionStrategy.ALL_GOALS,
                             2,
                             false,
                             "Let's start!",
                             0,
                             1 / 2);
        this.addInstructionsPhase.addGoal(new StartGoal());
        this.addPhase(addInstructionsPhase);
    }

    /**
     * Creates a phase in the mission where the objective is to locate the pools
     * of water and lava in the world
     */
    private void addApproachPoolsPhase() {
        RichContent instructions =
            RichContent.createFromJson("tutorial_instructions_pits.json");
        this.approachPoolsPhase = new MissionPhase(instructions,
                                                   CompletionStrategy.ALL_GOALS,
                                                   3,
                                                   true,
                                                   "Well Done!",
                                                   0,
                                                   1);
        this.approachPoolsPhase.addGoal(
            new ReachPositionGoal(-635, 4, 1582, 2));
        this.addPhase(approachPoolsPhase);
    }

    /**
     * Creates a phase in the mission where the objective is to craft a wooden
     * item in the world
     * TODO - Uncomment this code when the crafting phase needs to be
     * reactivated
     */
    //	private void addCraftItemPhase() {
    //		RichContent instructions =
    //				RichContent.createFromJson("tutorial_instruction_crafting.json");
    //		MissionPhase craftItemPhase = new MissionPhase(instructions,
    //				CompletionStrategy.ALL_GOALS,
    //				5,
    //				true,
    //				"Well Done!",
    //				0,
    //				2);
    //		craftItemPhase.addGoal(new CraftItemGoal(ItemType.WOODEN_AXE));
    //		this.addPhase(craftItemPhase);
    //	}

    /**
     * Creates a phase in the mission where the objective is to locate the
     * entities in the world
     */
    private void addApproachEntitiesPhase() {
        RichContent instructions =
            RichContent.createFromJson("tutorial_instructions_entities.json");
        MissionPhase approachEntitiesPhase =
            new MissionPhase(instructions,
                             CompletionStrategy.ALL_GOALS,
                             2,
                             true,
                             "Good job!",
                             0,
                             1);
        // approachEntitiesPhase.addGoal(new ReachPositionGoal(-615, 4, 1585,
        // 3));
        approachEntitiesPhase.addGoal(
            new ActivateButtonGoal(new BlockPos(-609, 5, 1586)));
        approachEntitiesPhase.addGoal(
            new ActivateButtonGoal(new BlockPos(-609, 5, 1590)));
        approachEntitiesPhase.addGoal(
            new ActivateButtonGoal(new BlockPos(-609, 5, 1594)));
        addPhase(approachEntitiesPhase);
    }

    /**
     * Creates a phase in the mission where the objective is to go to the center
     * of the arena
     */
    private void addEnterTheArenaPhase() {
        RichContent instructions =
            RichContent.createFromJson("tutorial_instructions_arena.json");
        this.enterTheArenaPhase = new MissionPhase(instructions,
                                                   CompletionStrategy.ALL_GOALS,
                                                   1,
                                                   true,
                                                   "Nice work!",
                                                   0,
                                                   1);
        this.enterTheArenaPhase.addGoal(
            new ReachPositionGoal(-623, 4, 1600, 2));
        this.addPhase(this.enterTheArenaPhase);
    }

    /**
     * Creates a phase in the mission where the objective is to battle and kill
     * a skeleton
     */
    private void addKillSkeletonPhase() {
        RichContent instructions =
            RichContent.createFromJson("tutorial_instructions_skeleton.json");
        this.killSkeletonPhase = new MissionPhase(instructions,
                                                  CompletionStrategy.ALL_GOALS,
                                                  0,
                                                  true,
                                                  "Well Done!",
                                                  0,
                                                  1);
        this.killSkeletonPhase.addGoal(new KillEntityGoal(this.skeletonUUID));
        this.addPhase(this.killSkeletonPhase);
    }

    /**
     * Creates a phase in the mission where the objective is to battle and kill
     * a zombie
     */
    private void addKillZombiePhase() {
        RichContent instructions =
            RichContent.createFromJson("tutorial_instructions_zombie.json");
        this.killZombiePhase = new MissionPhase(instructions,
                                                CompletionStrategy.ALL_GOALS,
                                                0,
                                                true,
                                                "Good work!",
                                                0,
                                                1);
        this.killZombiePhase.addGoal(new KillEntityGoal(this.zombieUUID));
        this.addPhase(this.killZombiePhase);
    }

    /**
     * Creates a phase in the mission where the objective is to save a villager
     */
    private void addSaveVillagerPhase() {
        RichContent instructions =
            RichContent.createFromJson("tutorial_instructions_villager.json");
        MissionPhase saveVillagerPhase =
            new MissionPhase(instructions,
                             CompletionStrategy.ALL_GOALS,
                             0,
                             true,
                             "You saved him!",
                             0,
                             2);
        saveVillagerPhase.addGoal(new ApproachEntityGoal(
            this.villagerUUID, MAX_DISTANCE_TO_SAVE_VILLAGER));
        this.addPhase(saveVillagerPhase);
    }

    /**
     * Creates a phase in the mission where the objective is leave the building
     */
    private void addLeaveTheBuildingPhase() {
        RichContent instructions =
            RichContent.createFromJson("tutorial_instructions_end.json");
        MissionPhase leaveTheBuildingPhase =
            new MissionPhase(instructions,
                             CompletionStrategy.ALL_GOALS,
                             0,
                             true,
                             "Well Done!",
                             0,
                             1);
        leaveTheBuildingPhase.addGoal(new ReachPositionGoal(-630, 4, 1574, 2));
        addPhase(leaveTheBuildingPhase);
    }

    @Override
    protected void updateScene(World world) {
        this.initializer.init(world);
        this.changePlayerPerspective();
        this.addMaterialsToUsersInventory(world);
        this.spawnExposedEntities(world);
        this.spawnSkeletonInTheArena(world);
        this.spawnZombieInsideTheBuilding(world);
        this.spawnVillagerInsideTheBuilding(world);
        this.respawnPlayersAfterDeath();
    }

    /**
     *  Cycles through Minecraft player perspectives
     */
    private void changePlayerPerspective() {
        if (this.viewTime <= 2 * SECONDS_PER_CAMERA_VIEW) {
            /* viewTime is in seconds. The player stays in each view mode for
             * 1.5 seconds. 20 Minecraft ticks equal 1 real second. viewTime is
             * incremented by 0.05 till 30 such ticks (1.5 second) have passed
             * for each view. */

            double roundedTime = Math.round(this.viewTime * 100.0) / 100.0;
            TomcatMessaging.TomcatMessage message =
                new TomcatMessage(TomcatMessageType.VIEW_CHANGED);

            if (roundedTime == 0.00) {
                TomcatClientServerHandler.sendMessageToAllClients(message,
                                                                  false);
                // third person back view
            }
            else if (roundedTime == SECONDS_PER_CAMERA_VIEW) {
                TomcatClientServerHandler.sendMessageToAllClients(message,
                                                                  false);
                // third person front view
            }
            else if (roundedTime == 2 * SECONDS_PER_CAMERA_VIEW) {
                TomcatClientServerHandler.sendMessageToAllClients(message,
                                                                  false);
                // first person view
            }
            this.viewTime += 0.05;
        }
    }

    private void spawnExposedEntities(World world) {
        if (this.shouldSpawnExposedEntities) {
            try {
                Drawing drawing = new Drawing();
                TomcatEntity skeleton = new TomcatEntity(
                    UUID.randomUUID(), -607, 4, 1592, EntityTypes.SKELETON);
                TomcatEntity zombie = new TomcatEntity(
                    UUID.randomUUID(), -607, 4, 1588, EntityTypes.ZOMBIE);
                TomcatEntity villager = new TomcatEntity(
                    UUID.randomUUID(), -607, 4, 1584, EntityTypes.VILLAGER);

                drawing.addObject(skeleton);
                drawing.addObject(zombie);
                drawing.addObject(villager);
                this.drawingHandler.draw(world, drawing);
            }
            catch (Exception e) {
                e.printStackTrace();
            }

            this.shouldSpawnExposedEntities = false;
        }
    }

    /**
     * Spawn skeleton in the arena
     * @param world - Minecraft world
     */
    private void spawnSkeletonInTheArena(World world) {
        if (this.shouldSpawnSkeletonInTheArena) {
            try {
                this.equipPlayers();
                Drawing drawing = new Drawing();
                TomcatEntity skeleton = new TomcatEntity(
                    this.skeletonUUID, -620, 4, 1596, EntityTypes.SKELETON);
                drawing.addObject(skeleton);
                this.drawingHandler.draw(world, drawing);
            }
            catch (Exception e) {
                e.printStackTrace();
            }
            this.shouldSpawnSkeletonInTheArena = false;
        }
    }

    /**
     * Give an axe to the player(s)
     */
    private void equipPlayers() {
        for (EntityPlayerMP player : MinecraftServerHelper.getPlayers()) {
            InventoryHandler.addItemToMainHand(player, ItemType.STONE_AXE);
        }
    }

    /**
     * Spawn zombie inside the building
     * @param world - Minecraft world
     */
    private void spawnZombieInsideTheBuilding(World world) {
        if (this.shouldSpawnZombieInsideTheBuilding) {
            try {
                Drawing drawing = new Drawing();

                TomcatEntity zombie = new TomcatEntity(
                    this.zombieUUID, -623, 4, 1571, EntityTypes.ZOMBIE);

                drawing.addObject(zombie);
                this.drawingHandler.draw(world, drawing);
            }
            catch (Exception e) {
                e.printStackTrace();
            }
            this.shouldSpawnZombieInsideTheBuilding = false;
        }
    }

    /**
     * Spawn villager inside the building
     * @param world - Minecraft world
     */
    private void spawnVillagerInsideTheBuilding(World world) {
        if (this.shouldSpawnVillagerInsideTheBuilding) {
            try {
                Drawing drawing = new Drawing();

                TomcatEntity villager = new TomcatEntity(
                    this.villagerUUID, -631, 4, 1570, EntityTypes.VILLAGER);

                drawing.addObject(villager);
                this.drawingHandler.draw(world, drawing);
            }
            catch (Exception e) {
                e.printStackTrace();
            }
            this.shouldSpawnVillagerInsideTheBuilding = false;
        }
    }

    /**
     * Add three planks and two sticks inside the player's inventory.
     * @param world - Minecraft world
     */
    private void addMaterialsToUsersInventory(World world) {
        if (this.shouldaddMaterialsToUsersInventory) {
            for (EntityPlayerMP player : MinecraftServerHelper.getServer()
                                             .getPlayerList()
                                             .getPlayers()) {
                InventoryHandler.addBlockToInventory(
                    player, BlockType.PLANKS, 3);
                InventoryHandler.addItemToInventory(player, ItemType.STICK, 2);
            }
            this.shouldaddMaterialsToUsersInventory = false;
        }
    }

    @Override
    protected void afterLastPhaseCompletion() {
        RichContent content =
            RichContent.createFromJson("tutorial_completion.json");
        TomcatMessageData messageData = new TomcatMessageData(content);
        TomcatMessaging.TomcatMessage message = new TomcatMessage(
            TomcatMessageType.SHOW_COMPLETION_SCREEN, messageData);
        TomcatClientServerHandler.sendMessageToAllClients(message, false);
        this.notifyAllAboutMissionEnding("0");
    }

    @Override
    public void goalAchieved(World world, MissionGoal goal) {
        if (goal instanceof ApproachEntityGoal) {
            this.handleVillagerRescue(world, (ApproachEntityGoal)goal);
        }
    }

    private void handleVillagerRescue(World world, ApproachEntityGoal goal) {
        this.addToDeletion(goal.getEntity(), world.getTotalWorldTime());

        TomcatMessaging.TomcatMessage message =
            new TomcatMessage(TomcatMessageType.VILLAGER_SAVED);
        TomcatClientServerHandler.sendMessageToAllClients(message, false);
    }

    @Override
    protected void onTimeOut() {
        // There's no timeout in the tutorial mission
    }

    @Override
    protected void beforePhaseTrasition() {
        if (this.currentPhase.equals(this.enterTheArenaPhase)) {
            this.shouldSpawnSkeletonInTheArena = true;
        }
        else if (this.currentPhase.equals(this.killSkeletonPhase)) {
            this.shouldSpawnZombieInsideTheBuilding = true;
        }
        else if (this.currentPhase.equals(this.killZombiePhase)) {
            this.shouldSpawnVillagerInsideTheBuilding = true;
        }
        else if (this.currentPhase.equals(this.approachPoolsPhase)) {
            /*
             * TODO - This should be commented out until the crafting phase
             * reactivated
             */
            // this.shouldaddMaterialsToUsersInventory = true;

            /*
             * TODO - The code below should be moved to a new condition block
             * after the crafting phase is reactivated, since the entities must
             * be spawned in the mission after the crafting phase instead of the
             * approaching pools one.
             */
            this.shouldSpawnExposedEntities = true;
        }
    }

    @Override
    public void setTimeLimitInSeconds(long timeLimitInSeconds) {
        // Tutorial mission has no time limit
        this.timeLimitInSeconds = -1;
    }

    @Override
    public PosAndDirection
    getPlayersInitialPositionAndDirection(EntityPlayerMP player) {
        PosAndDirection positionAndDirection = new PosAndDirection();
        positionAndDirection.setX(new BigDecimal(-623));
        positionAndDirection.setY(new BigDecimal(4));
        positionAndDirection.setZ(new BigDecimal(1584));
        return positionAndDirection;
    }

    @Override
    protected boolean hasSelfReport() {
        return false;
    }

    @Override
    protected SelfReportContent getSelfReportContent(EntityPlayerMP player,
                                                     World world) {
        return null;
    }

    @Override
    protected void onPlayerDeath(EntityPlayer player) {
        this.revivePlayer(player);
    }

    private void revivePlayer(EntityPlayer player) {
        player.isDead = false;
        player.setHealth(player.getMaxHealth());
        deadPlayers.add(player);
    }

    private void respawnPlayersAfterDeath() {
        for (Object playerObject : deadPlayers.toArray()) {
            EntityPlayer player = (EntityPlayer)playerObject;

            player.rotationYaw = 0;
            player.rotationPitch = 0;
            player.setPositionAndUpdate(-623, 4, 1584);

            if (player.isBurning()) {
                player.extinguish();
            }
            deadPlayers.remove(player);
        }
    }
}
