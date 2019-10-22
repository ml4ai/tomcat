package edu.arizona.tomcat.Mission;

import net.minecraft.world.World;

public class MissionHandler {
	
	private Mission mission;
	
	/**
	 * Sets the main mission
	 * @param id - Mission ID
	 * @param timeLimitInSeconds - Duration of the mission in seconds
	 */
	public void setMission(int id, int timeLimitInSeconds) {
		this.mission = MissionFactory.create(id);
		this.mission.setTimeLimitInSeconds(timeLimitInSeconds);
	}
	
	/**
	 * Initialize the main mission
	 * @param world
	 */
	public void initMission(World world) {
		this.mission.init(world);
	}
	
	/**
	 * Updates the main mission from time to time
	 * @param world
	 */
	public void updateMission(World world) {
		this.mission.update(world);
	}
	
}
