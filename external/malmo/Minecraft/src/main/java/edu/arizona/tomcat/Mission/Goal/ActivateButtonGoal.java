package edu.arizona.tomcat.Mission.Goal;

import net.minecraft.block.material.MapColor;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public class ActivateButtonGoal extends MissionGoal {

    private BlockPos buttonBlockPosition;

    /**
     * Constructor
     * @param buttonBlockPosition - Position of the block that contains the
     *     button
     */
    public ActivateButtonGoal(BlockPos buttonBlockPosition) {
        this.buttonBlockPosition = buttonBlockPosition;
    }

    @Override
    protected void updateGoalStatus(World world) {
        this.goalAchieved =
            (world.getBlockState(this.buttonBlockPosition).getMapColor() ==
             MapColor.LIME);
    }
}