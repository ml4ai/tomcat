package edu.arizona.tomcat.Mission;

import com.microsoft.Malmo.MalmoMod;
import com.microsoft.Malmo.Schemas.EntityTypes;
import com.microsoft.Malmo.Schemas.PosAndDirection;
import edu.arizona.tomcat.Messaging.TomcatMessageData;
import edu.arizona.tomcat.Messaging.TomcatMessaging;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessageType;
import edu.arizona.tomcat.Mission.Client.ClientMission;
import edu.arizona.tomcat.Mission.Client.SARClientMission;
import edu.arizona.tomcat.Mission.Goal.ApproachEntityGoal;
import edu.arizona.tomcat.Mission.Goal.MissionGoal;
import edu.arizona.tomcat.Mission.MissionPhase.CompletionStrategy;
import edu.arizona.tomcat.Mission.gui.RichContent;
import edu.arizona.tomcat.Mission.gui.SelfReportContent;
import edu.arizona.tomcat.Utils.Converter;
import edu.arizona.tomcat.Utils.MinecraftServerHelper;
import edu.arizona.tomcat.World.Building;
import edu.arizona.tomcat.World.Drawing;
import edu.arizona.tomcat.World.MultiRoomBuilding;
import edu.arizona.tomcat.World.TomcatEntity;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.world.World;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

public class SARMission extends Mission {

    public static final int NUMBER_OF_VILLAGERS = 4;
    private static final int MAX_DISTANCE_TO_SAVE_VILLAGER = 1;


    private List<Building> availableListOfBuildings;
    private boolean dynamicInitializationComplete;
    private UUID[] villagersIDs;
    private int numberOfVillagersSaved;

    public SARMission() {
        super();
        this.dynamicInitializationComplete = false;
        this.numberOfVillagersSaved = 0;
        this.availableListOfBuildings = initBuildingsSAR();
    }

    /**
     * This method will create a List of Buildings which will be used for
     * the SAR mission. The buildings included in the list are predefined and
     * contain both Building and MultiRoom Building instances.
     *
     * @return List<Building> An ArrayList of buildings being used for SAR
     */
    private static List<Building> initBuildingsSAR() {

        // Single Room Buildings
        Building singleRoomBuilding1 = new Building(93, 64, 53);
        Building singleRoomBuilding2 = new Building(46, 64, 47);
        Building singleRoomBuilding3 = new Building(52, 66, 89);

        //MultiRoom Buildings
        MultiRoomBuilding multiRoomBuilding1 = new MultiRoomBuilding(57, 64, 61);
        multiRoomBuilding1.addRoom(63, 64, 63);

        MultiRoomBuilding multiRoomBuilding2 = new MultiRoomBuilding(72, 64, 75);
        multiRoomBuilding2.addRoom(69, 64, 81);

        MultiRoomBuilding multiRoomBuilding3 = new MultiRoomBuilding(88, 64, 87);
        multiRoomBuilding3.addRoom(94, 64, 90);

        // Creating List
        Building[] arrayOfBuildings = {singleRoomBuilding1, singleRoomBuilding2, singleRoomBuilding3,
                multiRoomBuilding1, multiRoomBuilding2, multiRoomBuilding3};

        return new ArrayList<Building>(Arrays.asList(arrayOfBuildings));


    }

    /**
     * This will return a random type of enemy
     *
     * @return - EntityTypes
     */
    private static EntityTypes getRandomEnemy() {
        double randomize = Math.random();
        if (randomize > 0.7) {
            return EntityTypes.SKELETON;
        } else {
            return EntityTypes.ZOMBIE;
        }
    }

    /**
     * From an instance of MultiRoomBuilding, this method will pick
     * out a random room and return its coordinates in the form of an
     * array as {x,y,z}
     *
     * @param mlb - Instance of MultiRoomBuilding
     * @return int[] Integer array of size 3 with coordinates {x,y,z}
     */
    private static int[] getRandomRoom(MultiRoomBuilding mlb) {
        double randomize = Math.random();

        if (randomize > 0.5) {
            return new int[]{mlb.getX(), mlb.getY(), mlb.getZ()};
        } else {
            int randomIndex = (int) (Math.random()) * (mlb.getNumberOfAdditionalRooms());
            return mlb.getAdditionalRooms().get(randomIndex);

        }
    }

    @Override
    protected int getID() {
        return MissionFactory.SEARCH_AND_RESCUE;
    }

    @Override
    public void init(World world) {
        super.init(world);
    }

    @Override
    protected void beforePhaseTrasition() {
        // No action to be taken
    }

    @Override
    protected void afterLastPhaseCompletion() {
        this.removeAllEntities();
        RichContent content = RichContent.createFromJson("sar_completion.json");
        content.setTextPlaceholder(0, Integer.toString(this.numberOfVillagersSaved));
        MalmoMod.network.sendTo(new TomcatMessaging.TomcatMessage(TomcatMessageType.SHOW_COMPLETION_SCREEN, new TomcatMessageData(content)), MinecraftServerHelper.getFirstPlayer());
        this.notifyAllAboutMissionEnding("0");
        this.cleanup();
    }

    @Override
    protected void onTimeOut() {
        this.removeAllEntities();
        RichContent content = RichContent.createFromJson("sar_completion.json");
        content.setTextPlaceholder(0, Integer.toString(this.numberOfVillagersSaved));
        MalmoMod.network.sendTo(new TomcatMessaging.TomcatMessage(TomcatMessageType.SHOW_COMPLETION_SCREEN, new TomcatMessageData(content)), MinecraftServerHelper.getFirstPlayer());
        this.notifyAllAboutMissionEnding("1");
        this.cleanup();
    }

    public void removeAllEntities() {
        World world = MinecraftServerHelper.getServer().getEntityWorld();
        for (Entity entity : world.getLoadedEntityList()) {
            world.removeEntity(entity);
        }
    }

    @Override
    protected void createPhases() {
        this.createVillagersIDs();
        RichContent instructions = RichContent.createFromJson("sar_instructions.json");
        instructions.setTextPlaceholder(0, Integer.toString(NUMBER_OF_VILLAGERS));
        instructions.setTextPlaceholder(1, Converter.secondsToString(this.timeLimitInSeconds, true));
        MissionPhase rescueVillagersPhase = new MissionPhase(instructions, CompletionStrategy.ALL_GOALS, 0);
        for (int i = 0; i < NUMBER_OF_VILLAGERS; i++) {
            ApproachEntityGoal goal = new ApproachEntityGoal(this.villagersIDs[i], MAX_DISTANCE_TO_SAVE_VILLAGER);
            rescueVillagersPhase.addGoal(goal);
        }
        this.addPhase(rescueVillagersPhase);
    }

    /**
     * Create unique IDs for each one of the villagers
     */

    private void createVillagersIDs() {
        this.villagersIDs = new UUID[NUMBER_OF_VILLAGERS];
        for (int i = 0; i < NUMBER_OF_VILLAGERS; i++) {
            this.villagersIDs[i] = UUID.randomUUID();
        }
    }

    @Override
    protected void updateScene(World world) {
        this.doDynamicInitialization(world);
    }

    /**
     * Perform dynamic initializations in the mission
     *
     * @param world - Minecraft world
     */
    private void doDynamicInitialization(World world) {
        if (!this.dynamicInitializationComplete) {
            this.spawnEntities(world);
            this.addItemsToInventory(world);
            this.dynamicInitializationComplete = true;
        }
    }

    /**
     * This method will spawn the villagers and enemies for the mission randomly.
     * For complex buildings, however, it will place only one villager and fill the
     * rest of the rooms with enemies
     *
     * @param world - The Minecraft world
     */
    private void spawnEntities(World world) {
        try {
            Drawing drawing = new Drawing();

            //Spawn Villagers first
            for (int i = 0; i < NUMBER_OF_VILLAGERS; i++) {
                Building randomBuilding = getRandomBuilding();
                TomcatEntity villager;

                if (randomBuilding instanceof MultiRoomBuilding) {
                    int[] coordinates = getRandomRoom((MultiRoomBuilding) randomBuilding);
                    int x = coordinates[0], y = coordinates[1], z = coordinates[2];

                    villager = new TomcatEntity(this.villagersIDs[i], x, y, z, EntityTypes.VILLAGER);
                    ((MultiRoomBuilding) randomBuilding).markRoomAsFilled(coordinates); // Marking this room as used
                    
                    this.fillRemainingWithEnemies(drawing, (MultiRoomBuilding) randomBuilding);

                } else {
                    // Normal Building with one room so there's only one set of coordinates we can use
                    int x = randomBuilding.getX(), y = randomBuilding.getY(), z = randomBuilding.getZ();
                    villager = new TomcatEntity(this.villagersIDs[i], x, y, z, EntityTypes.VILLAGER);

                    randomBuilding.fillMainRoom();
                }
                drawing.addObject(villager);
                this.availableListOfBuildings.remove(randomBuilding);
            }

            // All rooms in all remaining buildings are filled with random enemies
            for (Building building : this.availableListOfBuildings) {
                if (building instanceof MultiRoomBuilding) {
                    fillRemainingWithEnemies(drawing, (MultiRoomBuilding) building);
                } else {
                    int x = building.getX(), y = building.getY(), z = building.getZ();
                    TomcatEntity enemy = new TomcatEntity(x, y, z, getRandomEnemy());
                    drawing.addObject(enemy);
                    building.fillMainRoom();
                }
            }

            this.drawingHandler.draw(world, drawing);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * When passed a multiroom building, this method will check for
     * empty rooms. Then, for all such empty rooms, it will extract the coordinate
     * of the room and add a random enemy in it through the drawing object.
     * <p>
     * This method only modified the Drawing instance and does not call the
     * drawingHandler
     *
     * @param drawing - An instance of Drawing
     * @param mlb     - An instance of a Multiroom building
     */
    private void fillRemainingWithEnemies(Drawing drawing, MultiRoomBuilding mlb) {
        if (mlb.mainRoomIsFilled()) {
            List<int[]> additionalRooms = mlb.getAdditionalRooms();
            List<int[]> filledAdditionalRooms = mlb.getFilledAdditionalRooms();

            for (int[] room : additionalRooms) {
                if (!filledAdditionalRooms.contains(room)) {
                    int x = room[0], y = room[1], z = room[2];
                    mlb.markRoomAsFilled(room); // We can now mark this room as filled

                    TomcatEntity enemy = new TomcatEntity(x, y, z, getRandomEnemy());
                    drawing.addObject(enemy);

                }
            }

        } else {
            // If the Main room is still empty
            int x = mlb.getX(), y = mlb.getY(), z = mlb.getZ();
            TomcatEntity enemy = new TomcatEntity(x, y, z, getRandomEnemy());

            drawing.addObject(enemy);
            mlb.fillMainRoom();

            // After filling the main room, it will recurse to fill any remaining additional rooms
            fillRemainingWithEnemies(drawing, mlb);
        }


    }

    /**
     * This method will return a random building from the global variable
     * which stores the available list of buildings. The
     * buildings returned may be an instance of MultiRoomBuilding.
     */
    private Building getRandomBuilding() {
        int randomIndex = (int) (Math.random()) * this.availableListOfBuildings.size();
        return this.availableListOfBuildings.get(randomIndex);

    }

    /**
     * Add items to the player's inventory to help them accomplish the mission goals
     *
     * @param world
     */
    private void addItemsToInventory(World world) {
        //InventoryHandler.addItemToInventory(ItemType.IRON_SWORD, 1);
    }

    @Override
    public void goalAchieved(World world, MissionGoal goal) {
        if (goal instanceof ApproachEntityGoal) {
            this.handleVillagerRescue(world, (ApproachEntityGoal) goal);
        }
    }

    private void handleVillagerRescue(World world, ApproachEntityGoal goal) {
        this.numberOfVillagersSaved++;
        this.addToDeletion(goal.getEntity(), world.getTotalWorldTime());
        MalmoMod.network.sendTo(new TomcatMessaging.TomcatMessage(TomcatMessageType.VILLAGER_SAVED), MinecraftServerHelper.getFirstPlayer());
    }

    @Override
    public ClientMission getClientMissionInstance() {
        return new SARClientMission();
    }

    @Override
    public PosAndDirection getPlayersInitialPositionAndDirection(EntityPlayerMP player) {
        PosAndDirection positionAndDirection = new PosAndDirection();
        positionAndDirection.setX(new BigDecimal(22));
        positionAndDirection.setY(new BigDecimal(64));
        positionAndDirection.setZ(new BigDecimal(73));
        positionAndDirection.setYaw(new BigDecimal(-90));
        return positionAndDirection;
    }

    @Override
    protected boolean hasSelfReport() {
        return true;
    }

    @Override
    protected SelfReportContent getSelfReportContent(World world) {
        String id = Long.toString(world.getTotalWorldTime());
        return SelfReportContent.createFromJson(id, "self_report1.json");
    }

    @Override
    protected void onPlayerDeath() {
        this.onTimeOut();
    }
}
