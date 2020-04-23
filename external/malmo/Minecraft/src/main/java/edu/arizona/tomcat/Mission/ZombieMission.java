package edu.arizona.tomcat.Mission;

import java.math.BigDecimal;
import java.util.UUID;

import com.microsoft.Malmo.Schemas.PosAndDirection;

import edu.arizona.tomcat.Messaging.TomcatClientServerHandler;
import edu.arizona.tomcat.Messaging.TomcatMessageData;
import edu.arizona.tomcat.Messaging.TomcatMessaging;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessage;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessageType;
import edu.arizona.tomcat.Mission.MissionPhase.CompletionStrategy;
import edu.arizona.tomcat.Mission.Goal.ApproachEntityGoal;
import edu.arizona.tomcat.Mission.Goal.MissionGoal;
import edu.arizona.tomcat.Mission.gui.RichContent;
import edu.arizona.tomcat.Mission.gui.SelfReportContent;
import edu.arizona.tomcat.Utils.Converter;
import edu.arizona.tomcat.Utils.MinecraftServerHelper;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.world.World;

public class ZombieMission extends Mission {

  public static final int NUMBER_OF_VILLAGERS = 4;
  private static final int MAX_DISTANCE_TO_SAVE_VILLAGER = 1;
    
  private UUID[] villagersIds;
  private int numberOfVillagersSaved;

  public ZombieMission() {
    super();
    this.id = ID.ZOMBIE;
    this.numberOfVillagersSaved = 0;
    this.createVillagersIDs();
  }

  @Override
  public void init(World world) {
    super.init(world);
    this.initializer = new ZombieMissionInitializer(this.levelOfDifficulty, this.villagersIds, this.drawingHandler);
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
    RichContent instructions =
        RichContent.createFromJson("sar_instructions.json");
    instructions.setTextPlaceholder(0, Integer.toString(NUMBER_OF_VILLAGERS));
    instructions.setTextPlaceholder(
        1, Converter.secondsToString(this.timeLimitInSeconds, true));
    MissionPhase rescueVillagersPhase =
        new MissionPhase(instructions, CompletionStrategy.ALL_GOALS, 0);
    for (int i = 0; i < NUMBER_OF_VILLAGERS; i++) {
      ApproachEntityGoal goal = new ApproachEntityGoal(
          this.villagersIds[i], MAX_DISTANCE_TO_SAVE_VILLAGER);
      rescueVillagersPhase.addGoal(goal);
    }
    this.addPhase(rescueVillagersPhase);
  }

  /**
   * Create unique IDs for each one of the villagers
   */

  private void createVillagersIDs() {
    this.villagersIds = new UUID[NUMBER_OF_VILLAGERS];
    for (int i = 0; i < NUMBER_OF_VILLAGERS; i++) {
      this.villagersIds[i] = UUID.randomUUID();
    }
  }

  @Override
  protected void updateScene(World world) {
    this.initializer.init(world);
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
