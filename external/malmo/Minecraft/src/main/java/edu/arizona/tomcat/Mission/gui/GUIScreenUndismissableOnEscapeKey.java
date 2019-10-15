package edu.arizona.tomcat.Mission.gui;

import java.io.IOException;

import net.minecraft.client.gui.GuiScreen;

public class GUIScreenUndismissableOnEscapeKey extends GuiScreen {
	
	@Override
	protected void keyTyped(char typedChar, int keyCode) throws IOException {
		// Does anything here. This is just to avoid the default behavior of closing the screen when ESC is typed. 
	}

}
