package edu.arizona.tomcat.Mission.gui;

import java.util.ArrayList;

import net.minecraft.client.gui.GuiButton;
import net.minecraft.client.gui.GuiLabel;

public class InstructionsScreen extends GUIScreenUndismissableOnEscapeKey {
	
	public static final int BUTTON_OK = 0;
	
	private ArrayList<String> instructions;
	private ArrayList<ScreenListener> listeners;
		
	public InstructionsScreen(ArrayList<String> instructions) {
		this.instructions = instructions;
		this.listeners = new ArrayList<ScreenListener>();
	}
	
	@Override
	public void initGui() {
		super.initGui();	
		
		GuiLabel instructionsLabel = new GuiLabel(fontRendererObj, 1, (this.width - 200) / 2, this.height / 4, 200, 20, 0xFFFFFF);
		instructionsLabel.setCentered();
		for(String pieceOfInstructions : this.instructions) {
			instructionsLabel.addLine(pieceOfInstructions);
		}
		this.labelList.add(instructionsLabel);
		this.buttonList.add(new GuiButton(BUTTON_OK, width/ 2 - 25, height / 2, 50, 20, "Ok"));
	}

	@Override
	protected void actionPerformed(GuiButton guiButton) {
		if(guiButton.id == BUTTON_OK) {
			this.dismissScreen(ScreenListener.ButtonType.OK);
		}
	}

	@Override
	public boolean doesGuiPauseGame() {
		return true;
	}
	
	/**
	 * Adds listener to be notified once one of the buttons is pressed
	 * @param listener - Object that wants to be notified when one of the buttons is pressed
	 */
	public void addListener(ScreenListener listener) {
		this.listeners.add(listener);
	}
	
	/**
	 * Close the GUI and notify listeners which of the buttons was pressed
	 * @param buttonType - Type of the button pressed
	 */
	public void dismissScreen(ScreenListener.ButtonType buttonType) {
		this.mc.player.closeScreen();
		for (ScreenListener listener : this.listeners) {
			listener.screenDismissed(this, buttonType);
		}		
	}	

}
