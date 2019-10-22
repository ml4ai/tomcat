package edu.arizona.tomcat.Mission.Goal;

import net.minecraft.world.World;

public abstract class MissionGoal {
	
	protected boolean goalAchieved;
	
	/**
	 * Keeps track of the goal progress at each game iteration
	 * @param world
	 */
	public abstract void update(World world);
	
	/**
	 * Indicates whether the goal has been achieved
	 * @return
	 */
	public boolean hasBeenAchieved() {
		return this.goalAchieved;
	}
	
	/**
	 * Reopens the goal if it was previously achieved.
	 */
	public void reset() {
		this.goalAchieved = false;
	}

}
