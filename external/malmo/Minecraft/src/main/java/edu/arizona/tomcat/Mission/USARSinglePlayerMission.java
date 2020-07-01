package edu.arizona.tomcat.Mission;

import com.microsoft.Malmo.Schemas.PosAndDirection;
import edu.arizona.tomcat.Messaging.TomcatClientServerHandler;
import edu.arizona.tomcat.Messaging.TomcatMessageData;
import edu.arizona.tomcat.Messaging.TomcatMessaging;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessage;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessageType;
import edu.arizona.tomcat.Mission.Goal.MissionGoal;
import edu.arizona.tomcat.Mission.gui.RichContent;
import edu.arizona.tomcat.Mission.gui.SelfReportContent;
import edu.arizona.tomcat.Utils.Converter;
import edu.arizona.tomcat.Utils.MinecraftServerHelper;
import java.math.BigDecimal;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.world.World;

public class USARSinglePlayerMission extends Mission {

    private int numberOfVillagersSaved;

    public USARSinglePlayerMission() {
        super();
        this.id = ID.USAR_SINGLE_PLAYER;
    }

    @Override
    public void init(World world) {
        super.init(world);
    }

    @Override
    protected void beforePhaseTrasition() {
        // No action to be taken
    }

    /**
     * Asks the clients to update the countdown
     * @param remainingSeconds
     */
    @Override
    protected void askClientsToUpdateCountdown(int remainingSeconds) {
        // NOOP
    }

    /**
     * Defines the duration of the mission in seconds
     * @param timeLimitInSeconds - Time in seconds until the end of the mission
     */
    public void setTimeLimitInSeconds(long timeLimitInSeconds) {
        // For the USAR Singleplayer mission we set the time limit to 900
        // seconds.
        this.timeLimitInSeconds = -1;
    }

    @Override
    protected void afterLastPhaseCompletion() {
        this.cleanup();
        this.removeAllEntities();
        RichContent content = RichContent.createFromJson("sar_completion.json");
        content.setTextPlaceholder(
            0, Integer.toString(this.numberOfVillagersSaved));
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
    protected void createPhases() {}

    @Override
    protected void updateScene(World world) {
        this.initializer.init(world);
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
        SelfReportContent selfReportContent =
            SelfReportContent.createFromJson("self_report_usar.json");
        return selfReportContent;
    }

    @Override
    protected void onPlayerDeath(EntityPlayer player) {
        this.onTimeOut();
    }
}
