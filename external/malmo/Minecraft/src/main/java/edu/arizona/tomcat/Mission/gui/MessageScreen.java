package edu.arizona.tomcat.Mission.gui;

import java.util.List;

public class MessageScreen extends InstructionsScreen {
	
	/**
	 * Constructor
	 */
	public MessageScreen() {
		super();
	}
	
	/**
	 * Constructor
	 * @param message - Message displayed on the screen
	 */
	public MessageScreen(List<String> message) {
		super(message);
	}
		
	@Override
	public void initGui() {
		this.drawText();
	}

	@Override
	public boolean doesGuiPauseGame() {
		return false;
	}

}
