package edu.arizona.tomcat.Mission;

import edu.arizona.tomcat.Mission.Goal.MissionGoal;
import net.minecraft.world.World;

public interface PhaseListener {

    /**
     * Notifies listeners about the phase completion
     */
    public void phaseCompleted();

    /**
     * Notifies listeners about a goal achievement
     */
    public void goalAchieved(World world, MissionGoal goal);
}
