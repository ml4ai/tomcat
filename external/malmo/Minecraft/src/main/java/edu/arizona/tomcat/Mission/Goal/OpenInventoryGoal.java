package edu.arizona.tomcat.Mission.Goal;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.gui.inventory.GuiInventory;
import net.minecraft.world.World;

public class OpenInventoryGoal extends MissionGoal {

  @Override
  public void updateGoalStatus(World world) {
    GuiScreen screen = Minecraft.getMinecraft().currentScreen;
    if (screen instanceof GuiInventory) {
      this.goalAchieved = true;
    }
  }
}
