package edu.arizona.tomcat.Mission.gui;

public class SARCompletionScreen extends MessageScreen {
	
	public SARCompletionScreen(int totalVillagersSaved) {
		super();		
		this.instructions.add("Mission Ended!");
		this.instructions.add("");
		this.instructions.add("Number of villagers saved: " + totalVillagersSaved);
	}	
	
	@Override
	public boolean doesGuiPauseGame() {
		return true;
	}

}
