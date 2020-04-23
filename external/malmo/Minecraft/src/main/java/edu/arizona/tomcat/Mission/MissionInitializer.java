package edu.arizona.tomcat.Mission;

import net.minecraft.world.World;

public class MissionInitializer {
	
	private boolean initialized;
	
	/**
	 * Constructor
	 */
	public MissionInitializer() {
		this.initialized = false;
	}
	
	/**
	 * Method called by a mission to dynamically initialize the world
	 * @param world
	 */
	public void init(World world) {
		if (!this.initialized) {
			this.updateWorldUponInit(world);
			this.initialized = true;
		}
	}
	
	/**
	 * Perform dynamic changes to the world in the beginning of the mission
	 */
	protected void updateWorldUponInit(World world) {};
	
}
