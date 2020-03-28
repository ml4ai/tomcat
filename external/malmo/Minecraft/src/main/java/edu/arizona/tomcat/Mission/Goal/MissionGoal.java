package edu.arizona.tomcat.Mission.Goal;

import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.world.World;

public abstract class MissionGoal {

  protected boolean goalAchieved;
  protected EntityPlayerMP player;

  /**
   * Keeps track of the goal progress at each game iteration
   * @param world - Minecraft world
   */
  public void update(World world) {
    if (!this.goalAchieved) {
      this.updateGoalStatus(world);
    }
  }

  /**
   * Indicates whether the goal has been achieved
   * @return
   */
  public boolean hasBeenAchieved() { return this.goalAchieved; }

  /**
   * Reopens the goal if it was previously achieved.
   */
  public void reset() { this.goalAchieved = false; }

  /**
   * Update goal status if it was accomplished
   * @param world - Minecraft world
   */
  protected abstract void updateGoalStatus(World world);
  
  /**
   * Retrieves the player who accomplished the goal
   * @return
   */
  public EntityPlayerMP getPlayer() { return this.player; }
}
