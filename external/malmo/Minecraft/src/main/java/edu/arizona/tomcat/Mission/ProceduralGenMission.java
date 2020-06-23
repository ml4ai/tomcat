package edu.arizona.tomcat.Mission;

import com.microsoft.Malmo.Schemas.PosAndDirection;
import edu.arizona.tomcat.Mission.Goal.MissionGoal;
import edu.arizona.tomcat.Mission.gui.SelfReportContent;
import jdk.nashorn.internal.parser.JSONParser;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.init.Blocks;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;


import java.math.BigDecimal;

public class ProceduralGenMission extends Mission {

    public boolean shouldBuild;

    public ProceduralGenMission() {
        super();
        this.id = ID.PROCEDURAL;
        this.shouldBuild = true;
    }

    private void buildStructures(World world) {
        if (this.shouldBuild == true) {

            BlockPos pos1 = new BlockPos(0, 0, 0);
            BlockPos pos2 = new BlockPos(5, 5, 5);
            this.placeAABB(world, pos1, pos2, false);

            pos1 = new BlockPos(10, 0, 0);
            pos2 = new BlockPos(15, 5, 5);
            this.placeAABB(world, pos1, pos2, true);

            this.shouldBuild = false;
        } else {
            return;
        }

    }

    private void placeAABB(World world, BlockPos pos1, BlockPos pos2, boolean isHollow) {
        int x1 = pos1.getX(), y1 = pos1.getY(), z1 = pos1.getZ();
        int x2 = pos2.getX(), y2 = pos2.getY(), z2 = pos2.getZ();

        for (int x = x1; x <= x2; x++) {
            for (int y = y1; y <= y2; y++) {
                for (int z = z1; z <= z2; z++) {
                    if (isHollow) {
                        if (x == x1 || x == x2 || z == z1 || z == z2) {
                            BlockPos pos = new BlockPos(x, 4 + y, z);
                            world.setBlockState(pos, Blocks.DIAMOND_BLOCK.getDefaultState());
                        }
                    }else{
                        BlockPos pos = new BlockPos(x, 4 + y, z);
                        world.setBlockState(pos, Blocks.DIAMOND_BLOCK.getDefaultState());
                    }
                }
            }
        }
    }

    private void parseJSON(){
        // TODO Implement this

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
    protected void updateScene(World world) {
        this.initializer.init(world);
        this.buildStructures(world);
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
