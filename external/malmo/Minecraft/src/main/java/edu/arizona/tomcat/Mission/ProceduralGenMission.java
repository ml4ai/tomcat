package edu.arizona.tomcat.Mission;

import com.microsoft.Malmo.Schemas.PosAndDirection;
import edu.arizona.tomcat.Mission.Goal.MissionGoal;
import edu.arizona.tomcat.Mission.gui.SelfReportContent;
import edu.arizona.tomcat.Utils.WorldReader;
import edu.arizona.tomcat.World.Drawing;
import edu.arizona.tomcat.World.DrawingHandler;
import edu.arizona.tomcat.World.TomcatEntity;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import net.minecraft.block.BlockDoor;
import net.minecraft.block.state.IBlockState;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.omg.PortableInterceptor.SYSTEM_EXCEPTION;

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
            // Grab map representing world
            WorldReader worldReader = new WorldReader("low_level_map.json");

            Drawing drawing = new Drawing();
            Map<BlockPos, IBlockState> worldMap = worldReader.getBlocksMap();
            List<TomcatEntity> entityList = worldReader.getEntityList();

            // Place blocks
            for (Map.Entry<BlockPos, IBlockState> entry : worldMap.entrySet()) {
                world.setBlockState(entry.getKey(), entry.getValue());

                // If it is a door and the block adjacent to it by -1 along z is
                // an air block We close the door. Fixes Minecraft's confusion
                // about what a closed and open door is
                if (entry.getValue().equals(
                        Blocks.DARK_OAK_DOOR.getStateFromMeta(9)) ||
                    entry.getValue().equals(
                        Blocks.DARK_OAK_DOOR.getStateFromMeta(0))) {
                    BlockPos pos = entry.getKey().add(0, 0, -2);
                    if (world.isAirBlock(pos)) {
                        Blocks.DARK_OAK_DOOR.toggleDoor(
                            world, entry.getKey(), true);
                    }
                }
            }

            for (TomcatEntity entity : entityList) {
                drawing.addObject(entity);
            }

            try {
                this.drawingHandler.draw(world, drawing);
            }
            catch (Exception e) {
                e.printStackTrace();
            }

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
