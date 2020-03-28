package edu.arizona.tomcat.Mission.Goal;

import edu.arizona.tomcat.Utils.MinecraftServerHelper;
import net.minecraft.entity.player.EntityPlayerMP;
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
  public void updateGoalStatus(World world) {
	  for (EntityPlayerMP player : MinecraftServerHelper.getServer().getPlayerList().getPlayers()) {
		  this.goalAchieved = player.getDistanceSq(this.x, this.y, this.z) < Math.pow(this.range, 2);
		  
		  if (this.goalAchieved) {
	          this.player = player;
	          break;
	      }
	  }
  }
}
