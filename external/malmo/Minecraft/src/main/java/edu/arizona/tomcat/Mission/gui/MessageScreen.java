package edu.arizona.tomcat.Mission.gui;

import net.minecraft.client.gui.GuiLabel;

public class MessageScreen extends GUIScreenUndismissableOnEscapeKey {
	
	private static final int MARGIN = 20;
	private static final int TEXT_PADDING = 10;
	private static final int TEXT_HEIGHT = 30;
	private static final int BACKGROUND_COLOR = 0xEEFFFFFF;
	private static final int FOREGORUND_COLOR = 0x00000000;
	
	private String message;
	
	/**
	 * Constructor
	 * @param message - Message displayed on the screen
	 */
	public MessageScreen(String message) {
		super();
		this.message = message;
	}
		
	@Override
	public void initGui() {
		this.addMessageToTheScreen();
	}
	
	/**
	 * Add message to the screen. It will be actually drawn when the method drawScreen is called from the superclass
	 */
	private void addMessageToTheScreen() {
		int textWidth = this.width - 2*(MARGIN + TEXT_PADDING);
		int textHeight = TEXT_HEIGHT;
		int x = MARGIN + TEXT_PADDING;
		int y = MARGIN + TEXT_PADDING;

		GuiLabel guiLabel = new UnshadowedLabel(this.fontRendererObj, 1, x, y, textWidth, textHeight, FOREGORUND_COLOR);
		guiLabel.setCentered();
		guiLabel.addLine(this.message);
		
		this.labelList.add(guiLabel);
	}
	
	@Override
	public void drawScreen(int mouseX, int mouseY, float partialTicks) {
		this.drawBackground();		
		super.drawScreen(mouseX, mouseY, partialTicks);
	}
	
	/**
	 * Draw screen background
	 */
	private void drawBackground() {
		drawRect(MARGIN, MARGIN, this.width - MARGIN, TEXT_HEIGHT + 2*TEXT_PADDING + MARGIN, BACKGROUND_COLOR);
	}

	@Override
	public boolean doesGuiPauseGame() {
		return false;
	}

}
