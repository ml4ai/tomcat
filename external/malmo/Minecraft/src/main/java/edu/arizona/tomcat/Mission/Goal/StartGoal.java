package edu.arizona.tomcat.Mission.Goal;

import net.minecraft.world.World;

public class StartGoal extends MissionGoal {

    @Override
    protected void updateGoalStatus(World world) {
        // TODO Auto-generated method stub
        this.goalAchieved = true;
    }
}
