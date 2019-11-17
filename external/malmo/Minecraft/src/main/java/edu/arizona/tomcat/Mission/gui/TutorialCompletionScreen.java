package edu.arizona.tomcat.Mission.gui;

public class TutorialCompletionScreen extends MessageScreen {
	
	public TutorialCompletionScreen() {
		super();
		this.instructions.add("Congratulations!");
		this.instructions.add("You completed all the steps in this mission.");
		this.instructions.add("Now you are prepared to face an exciting adventure!");
	}
	
	@Override
	public boolean doesGuiPauseGame() {
		return true;
	}

}
