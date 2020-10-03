package edu.arizona.tomcat.Mission;

import com.microsoft.Malmo.Schemas.PosAndDirection;
import edu.arizona.tomcat.Mission.Goal.MissionGoal;
import edu.arizona.tomcat.Mission.gui.SelfReportContent;
import edu.arizona.tomcat.Utils.WorldBuilder;
import edu.arizona.tomcat.World.DrawingHandler;

import java.math.BigDecimal;

import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.world.World;

public class ProceduralGenMission extends Mission {

    private boolean shouldBuild;

    public ProceduralGenMission() {
        super();
        this.drawingHandler = DrawingHandler.getInstance();
        this.id = ID.PROCEDURAL;
        this.shouldBuild = true;
    }

    @Override
    protected void updateScene(World world) {
        this.initializer.init(world);
        this.buildStructures(world);
    }

    /**
     * This grabs the blueprint from the low_level_map.json file and uses a
     * WorldReader object to parse it. It then uses the resulting hashmap to
     * place all the blocks at the right places.
     *
     * @param world The world in which the blocks are to be placed.
     */
    private void buildStructures(World world) {
        if (this.shouldBuild) {

            WorldBuilder worldBuilder = new WorldBuilder();
            worldBuilder.build("low_level_map.json", world);
            this.shouldBuild = false;
        }
    }

    @Override
    protected void createPhases() {
        // No action to be taken
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
    public void setTimeLimitInSeconds(long timeLimitInSeconds) {
        this.timeLimitInSeconds = -1;
    }

    @Override
    public PosAndDirection
    getPlayersInitialPositionAndDirection(EntityPlayerMP player) {
        PosAndDirection positionAndDirection = new PosAndDirection();
        positionAndDirection.setX(new BigDecimal(0));
        positionAndDirection.setY(new BigDecimal(4));
        positionAndDirection.setZ(new BigDecimal(0));
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
    public void goalAchieved(World world, MissionGoal goal) {
        // No action to be taken
    }

    @Override
    protected void onPlayerDeath(EntityPlayer player) {
        this.onTimeOut();
    }
}
