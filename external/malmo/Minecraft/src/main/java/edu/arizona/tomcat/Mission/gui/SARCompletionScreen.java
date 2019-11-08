package edu.arizona.tomcat.Mission.gui;


import java.util.ArrayList;


public class SARCompletionScreen extends InstructionsScreen {
	
	public SARCompletionScreen(int totalVillagersSaved) {
		super(null);
		this.instructions = new ArrayList<String>();		
		this.instructions.add("Mission Ended!");
		this.instructions.add("");
		this.instructions.add("Number of villagers saved: " + totalVillagersSaved);
	}
	
	@Override
	public void initGui() {		
		this.drawText();			
	}
	
	@Override
	public boolean doesGuiPauseGame() {
		return true;
	}

}
