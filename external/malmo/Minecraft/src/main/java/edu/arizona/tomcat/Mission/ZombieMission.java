package edu.arizona.tomcat.Mission;

import com.microsoft.Malmo.Schemas.EntityTypes;
import com.microsoft.Malmo.Schemas.PosAndDirection;
import edu.arizona.tomcat.Messaging.TomcatClientServerHandler;
import edu.arizona.tomcat.Messaging.TomcatMessageData;
import edu.arizona.tomcat.Messaging.TomcatMessaging;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessage;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessageType;
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
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public class ZombieMission extends Mission {

  public static final int NUMBER_OF_VILLAGERS = 4;
  private static final int MAX_DISTANCE_TO_SAVE_VILLAGER = 1;

  private List<Building> availableListOfBuildings;
  private boolean dynamicInitializationComplete;
  private UUID[] villagersIDs;
  private int numberOfVillagersSaved;

  public ZombieMission() {
    super();
    this.id = ID.ZOMBIE;
    this.dynamicInitializationComplete = false;
    this.numberOfVillagersSaved = 0;
    this.availableListOfBuildings = initBuildingsZombie();
  }

  /**
   * This method will create a List of Buildings which will be used for
   * the Zombie mission. The buildings included in the list are predefined and
   * contain both Building and MultiRoom Building instances.
   *
   * @return List<Building> An ArrayList of buildings being used for Zombie
   */
  private static List<Building> initBuildingsZombie() {

    // Single Room Buildings
    Building singleRoomBuilding1 = new Building(93, 64, 53);
    Building singleRoomBuilding2 = new Building(46, 64, 47);
    Building singleRoomBuilding3 = new Building(52, 67, 89);

    // MultiRoom Buildings
    MultiRoomBuilding multiRoomBuilding1 = new MultiRoomBuilding(57, 64, 61);
    multiRoomBuilding1.addRoom(63, 64, 63);

    MultiRoomBuilding multiRoomBuilding2 = new MultiRoomBuilding(72, 64, 75);
    multiRoomBuilding2.addRoom(69, 64, 81);

    MultiRoomBuilding multiRoomBuilding3 = new MultiRoomBuilding(88, 64, 87);
    multiRoomBuilding3.addRoom(94, 64, 90);

    // Creating List
    Building[] arrayOfBuildings = {singleRoomBuilding1,
                                   singleRoomBuilding2,
                                   singleRoomBuilding3,
                                   multiRoomBuilding1,
                                   multiRoomBuilding2,
                                   multiRoomBuilding3};

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
    }
    else {
      return EntityTypes.ZOMBIE;
    }
  }

  /**
   * From a Building object his method will pick
   * out a random room and return its coordinates in the form of a BlockPos
   * object. <p> If the Building has only one room, just the main room
   * coordinates will be returned since that is the only option.
   *
   * @param building - Any Building object
   * @return BlockPos - Coordinates of the random room
   */
  private static BlockPos getRandomRoom(Building building) {
    if (building instanceof MultiRoomBuilding) {
      double randomize = Math.random();

      if (randomize > 0.5) {
        return building.getMainRoomCoordinates();
      }
      else {
        int randomIndex =
            (int)(Math.random()) *
            (((MultiRoomBuilding)building).getNumberOfAdditionalRooms());
        return ((MultiRoomBuilding)building)
            .getAdditionalRooms()
            .get(randomIndex);
      }
    }
    else {
      return building.getMainRoomCoordinates();
    }
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
    this.cleanup();
    this.removeAllEntities();
    RichContent content = RichContent.createFromJson("sar_completion.json");
    content.setTextPlaceholder(0,
                               Integer.toString(this.numberOfVillagersSaved));
    TomcatMessageData messageData = new TomcatMessageData(content);
    TomcatMessaging.TomcatMessage message = new TomcatMessage(
        TomcatMessageType.SHOW_COMPLETION_SCREEN, messageData);
    TomcatClientServerHandler.sendMessageToAllClients(message, false);
    this.notifyAllAboutMissionEnding("0");
  }

  @Override
  protected void onTimeOut() {
    this.cleanup();
    this.removeAllEntities();
    RichContent content = RichContent.createFromJson("sar_completion.json");
    content.setTextPlaceholder(0,
                               Integer.toString(this.numberOfVillagersSaved));
    TomcatMessageData messageData = new TomcatMessageData(content);
    TomcatMessaging.TomcatMessage message = new TomcatMessage(
        TomcatMessageType.SHOW_COMPLETION_SCREEN, messageData);
    TomcatClientServerHandler.sendMessageToAllClients(message, false);
    this.notifyAllAboutMissionEnding("1");
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
    RichContent instructions =
        RichContent.createFromJson("sar_instructions.json");
    instructions.setTextPlaceholder(0, Integer.toString(NUMBER_OF_VILLAGERS));
    instructions.setTextPlaceholder(
        1, Converter.secondsToString(this.timeLimitInSeconds, true));
    MissionPhase rescueVillagersPhase =
        new MissionPhase(instructions, CompletionStrategy.ALL_GOALS, 0);
    for (int i = 0; i < NUMBER_OF_VILLAGERS; i++) {
      ApproachEntityGoal goal = new ApproachEntityGoal(
          this.villagersIDs[i], MAX_DISTANCE_TO_SAVE_VILLAGER);
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
   * <p>
   * For complex buildings with more than one room, it will place only one
   * villager in some room and fill the rest of the rooms with enemies
   *
   * @param world - The Minecraft world
   */
  private void spawnEntities(World world) {
    try {
      Drawing drawing = new Drawing();

      // Spawn Villagers first
      for (int i = 0; i < NUMBER_OF_VILLAGERS; i++) {
        Building randomBuilding = getRandomBuilding();

        BlockPos coordinates = getRandomRoom(randomBuilding);
        int x = coordinates.getX(), y = coordinates.getY(),
            z = coordinates.getZ();

        TomcatEntity villager = new TomcatEntity(
            this.villagersIDs[i], x, y, z, EntityTypes.VILLAGER);
        randomBuilding.markRoomAsFilled(
            coordinates); // Marking this room as used.
        // The building type doesn't matter above because of the overridden
        // methods for marking filled rooms.

        if (randomBuilding instanceof MultiRoomBuilding) {
          this.fillRemainingWithEnemies(drawing, (randomBuilding));
        }

        drawing.addObject(villager);
        this.availableListOfBuildings.remove(randomBuilding);
      }

      // All rooms in all remaining buildings are filled with random enemies
      for (Building building : this.availableListOfBuildings) {
        this.fillRemainingWithEnemies(drawing, building);
      }

      this.drawingHandler.draw(world, drawing);
    }
    catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * When passed a Building, this method will check for
   * empty rooms. Then, for all such empty rooms, it will extract the coordinate
   * of the room and add a random enemy in it through the drawing object.
   * <p>
   * This method can handle both single and MultiRoom Buildings.
   * <p>
   * This method only modifies the Drawing instance and does not call the
   * drawingHandler
   *
   * @param drawing  - An instance of Drawing
   * @param building - An instance of a Building
   */
  private void fillRemainingWithEnemies(Drawing drawing, Building building) {
    if (building instanceof MultiRoomBuilding && building.mainRoomIsFilled()) {
      List<BlockPos> additionalRooms =
          ((MultiRoomBuilding)building).getAdditionalRooms();
      List<BlockPos> filledAdditionalRooms =
          ((MultiRoomBuilding)building).getFilledAdditionalRooms();

      for (BlockPos room : additionalRooms) {
        if (!filledAdditionalRooms.contains(room)) {
          int x = room.getX(), y = room.getY(), z = room.getZ();
          building.markRoomAsFilled(
              room); // We can now mark this room as filled

          TomcatEntity enemy = new TomcatEntity(x, y, z, getRandomEnemy());
          drawing.addObject(enemy);
        }
      }
    }
    else if (!building.mainRoomIsFilled()) {
      // If the Main room is still empty for either a normal or MultiRoom
      // Building
      BlockPos mainRoomCoords = building.getMainRoomCoordinates();
      int x = mainRoomCoords.getX(), y = mainRoomCoords.getY(),
          z = mainRoomCoords.getZ();
      TomcatEntity enemy = new TomcatEntity(x, y, z, getRandomEnemy());

      drawing.addObject(enemy);
      building.fillMainRoom();

      // After filling the main room, it will recurse to fill any remaining
      // additional rooms
      fillRemainingWithEnemies(drawing, building);
    }
    else {
      ; // pass. This is when a normal building has it's main room filled. It
        // ends the recursive call for that case.
    }
  }

  /**
   * This method will return a random building from the global variable
   * which stores the available list of buildings. The
   * buildings returned may be an instance of MultiRoomBuilding.
   *
   * @return Building - An instance of Building which may be either a normal or
   * MultiRoom Building
   */
  private Building getRandomBuilding() {
    int randomIndex =
        (int)(Math.random() * this.availableListOfBuildings.size());
    return this.availableListOfBuildings.get(randomIndex);
  }

  /**
   * Add items to the player's inventory to help them accomplish the mission
   * goals
   *
   * @param world
   */
  private void addItemsToInventory(World world) {
    // InventoryHandler.addItemToInventory(ItemType.IRON_SWORD, 1);
  }

  @Override
  public void goalAchieved(World world, MissionGoal goal) {
    if (goal instanceof ApproachEntityGoal) {
      this.handleVillagerRescue(world, (ApproachEntityGoal)goal);
    }
  }

  private void handleVillagerRescue(World world, ApproachEntityGoal goal) {
    this.numberOfVillagersSaved++;
    this.addToDeletion(goal.getEntity(), world.getTotalWorldTime());

    TomcatMessaging.TomcatMessage message =
        new TomcatMessage(TomcatMessageType.VILLAGER_SAVED);
    TomcatClientServerHandler.sendMessageToAllClients(message, false);
  }

  @Override
  public PosAndDirection
  getPlayersInitialPositionAndDirection(EntityPlayerMP player) {
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
  protected SelfReportContent getSelfReportContent(EntityPlayerMP player,
                                                   World world) {
    SelfReportContent selfReportContent =
        SelfReportContent.createFromJson("self_report1.json");
    selfReportContent.setTextPlaceholder(
        0, Converter.secondsToString(this.getRemainingSeconds(world), false));
    selfReportContent.setTextPlaceholder(
        1, String.format("%.2f", player.getHealth()));
    selfReportContent.setTextPlaceholder(
        2, String.format("%.2f", player.getMaxHealth()));
    selfReportContent.setTextPlaceholder(
        3, Integer.toString(this.numberOfVillagersSaved));
    selfReportContent.setTextPlaceholder(4,
                                         Integer.toString(NUMBER_OF_VILLAGERS));
    return selfReportContent;
  }

  @Override
  protected void onPlayerDeath(EntityPlayer plauer) {
    this.onTimeOut();
  }
}
