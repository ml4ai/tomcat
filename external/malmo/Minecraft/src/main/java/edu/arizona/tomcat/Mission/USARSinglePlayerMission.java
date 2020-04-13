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
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public class USARSinglePlayerMission extends Mission {

  private List<Building> availableListOfBuildings;
  private boolean dynamicInitializationComplete;
  private int numberOfVillagersSaved;

  public USARSinglePlayerMission() {
    super();
    this.id = ID.USAR_SINGLE_PLAYER;
    this.dynamicInitializationComplete = false;
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
    RichContent instructions =
        RichContent.createFromJson("sar_instructions.json");
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
      this.addItemsToInventory(world);
      this.dynamicInitializationComplete = true;
    }
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
  public void goalAchieved(World world, MissionGoal goal) {}

  @Override
  public PosAndDirection
  getPlayersInitialPositionAndDirection(EntityPlayerMP player) {
    PosAndDirection positionAndDirection = new PosAndDirection();
    positionAndDirection.setX(new BigDecimal(-2186));
    positionAndDirection.setY(new BigDecimal(52));
    positionAndDirection.setZ(new BigDecimal(177));
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
    String id = Long.toString(world.getTotalWorldTime());
    SelfReportContent selfReportContent =
        SelfReportContent.createFromJson(id, "self_report1.json");
    selfReportContent.setTextPlaceholder(
        0, Converter.secondsToString(this.getRemainingSeconds(world), false));
    selfReportContent.setTextPlaceholder(
        1, String.format("%.2f", player.getHealth()));
    selfReportContent.setTextPlaceholder(
        2, String.format("%.2f", player.getMaxHealth()));
    return selfReportContent;
  }

  @Override
  protected void onPlayerDeath(EntityPlayer player) {
    this.onTimeOut();
  }
}
