package edu.arizona.tomcat.Mission.gui;


import java.util.ArrayList;
import java.util.List;

import com.microsoft.Malmo.MalmoMod;

import net.minecraft.client.gui.GuiButton;
import net.minecraft.client.gui.GuiLabel;
import net.minecraft.util.ResourceLocation;


public class InstructionsScreen extends GUIScreenUndismissableOnEscapeKey {
	
	private static final int LABEL_Y = 10;
	private static final int LABEL_WIDTH = 400;
	private static final int LABEL_HEIGHT = 100;
	private static final int BUTTON_WIDTH = 50;
	private static final int BUTTON_HEIGHT = 20;
	public static final int BUTTON_OK = 0;
	
	protected List<String> instructions;
	private ArrayList<ScreenListener> listeners;
	private String image;
	
	public InstructionsScreen(List<String> instructions) {
		this.instructions = instructions;
		this.listeners = new ArrayList<ScreenListener>();
	}
	
	public InstructionsScreen(ArrayList<String> instructions, String image) {
		this.instructions = instructions;
		this.image = image;
		this.listeners = new ArrayList<ScreenListener>();
	}
	
	@Override
	public void initGui() {
		super.initGui();	
		this.drawText();
		this.drawImage();
		this.drawButton();		
	}
	
	/**
	 * Draw text on the screen
	 */
	protected void drawText() {
		int labelX = (this.width - LABEL_WIDTH) / 2;
		
		GuiLabel instructionsLabel = new BoxLabel(this.fontRendererObj, 1, labelX, LABEL_Y, LABEL_WIDTH, LABEL_HEIGHT, 
				BoxLabel.COLOR_BLACK, BoxLabel.COLOR_WHITE);
		instructionsLabel.setCentered();
		
		for(String pieceOfInstructions : this.instructions) {
			instructionsLabel.addLine(pieceOfInstructions);
		}
		this.labelList.add(instructionsLabel);
	}
	
	/**
	 * Draw image on the screen
	 */
	protected void drawImage() {
		if(this.image != null) {			
			this.mc.getTextureManager().bindTexture(new ResourceLocation(MalmoMod.MODID, "textures/" + this.image));
			this.drawTexturedModalRect(100, LABEL_Y + LABEL_HEIGHT + 10, 0, 0, 256, 256);
		}
	}
	
	/**
	 * Draw button on the screen
	 */
	protected void drawButton() {
		int buttonX = width/ 2 - 25;
		int buttonY = LABEL_Y + LABEL_HEIGHT + 50;
		this.buttonList.add(new GuiButton(BUTTON_OK, buttonX, buttonY, BUTTON_WIDTH, BUTTON_HEIGHT, "Ok"));
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
