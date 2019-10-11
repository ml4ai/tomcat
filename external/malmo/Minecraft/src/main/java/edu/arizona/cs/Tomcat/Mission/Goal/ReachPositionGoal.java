package edu.arizona.cs.Tomcat.Mission.Goal;

import java.util.logging.Level;

import com.microsoft.Malmo.Utils.TCPUtils;

import net.minecraft.client.Minecraft;
import net.minecraft.world.World;

public class ReachPositionGoal extends MissionGoal {
	
	private int x;
	private int y;
	private int z;
	private int range;
	
	/**
	 * Constructor
	 * @param x - Position in the x axis 
	 * @param y - Position in the y axis
	 * @param z - Position in the z axis
	 * @param range - Tolerance 
	 */
	public ReachPositionGoal(int x, int y, int z, int range) {
		this.x = x;
		this.y = y;
		this.z = z;
		this.range = range;
	}

	@Override
	public void update(World world) {
		if (!this.goalAchieved) {
			TCPUtils.Log(Level.INFO, "Updating goal");
			this.goalAchieved = Minecraft.getMinecraft().player.getDistanceSq(this.x, this.y, this.z) < (this.range * this.range);
			TCPUtils.Log(Level.INFO, "Goal : (" + this.x + ", " + this.y + ", " + this.z + ")" + "Player : (" + Minecraft.getMinecraft().player.posX + ", " + Minecraft.getMinecraft().player.posY + ", " + Minecraft.getMinecraft().player.posZ + ")");
		}
	}
	

}
