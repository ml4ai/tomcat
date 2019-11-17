package edu.arizona.tomcat.Mission.gui;

import net.minecraft.client.gui.GuiScreen;

public interface ScreenListener {
	
	public static enum ButtonType { OK };
	
	/**
	 * Handles actions after a button was pressed in a screen
	 */
	public void screenDismissed(GuiScreen screen);
	
}
