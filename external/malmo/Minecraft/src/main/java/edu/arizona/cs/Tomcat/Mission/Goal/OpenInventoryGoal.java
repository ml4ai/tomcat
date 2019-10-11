package edu.arizona.cs.Tomcat.Mission.Goal;

import java.util.logging.Level;

import com.microsoft.Malmo.Utils.TCPUtils;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.gui.inventory.GuiInventory;
import net.minecraft.world.World;

public class OpenInventoryGoal extends MissionGoal {

	@Override
	public void update(World world) {
		TCPUtils.Log(Level.INFO, "Updating inventory goal");
		if (!this.goalAchieved) {
			GuiScreen screen = Minecraft.getMinecraft().currentScreen;
			if(screen != null)
				TCPUtils.Log(Level.INFO, screen.toString());
			else
				TCPUtils.Log(Level.INFO, "NULL screen");
			if (screen instanceof GuiInventory) {
				this.goalAchieved = true;
				
			}
		}
	}

}
