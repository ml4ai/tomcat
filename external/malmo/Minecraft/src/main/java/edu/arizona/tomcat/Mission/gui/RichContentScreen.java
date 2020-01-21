package edu.arizona.tomcat.Mission.gui;


import java.util.ArrayList;
import java.util.Iterator;

import net.minecraft.client.gui.GuiButton;
import net.minecraft.client.gui.GuiLabel;


public class RichContentScreen extends GUIScreenUndismissableOnEscapeKey {

	private static final int MARGIN = 10;
	private static final int TEXT_PADDING = 10;
	private static final int TEXT_HEIGHT = 80;
	private static final int CAPTION_VERTICAL_SPACING = 5;
	private static final int CAPTION_HEIGHT = 10;
	private static final int BUTTON_WIDTH = 50;
	private static final int BUTTON_HEIGHT = 20;
	private static final int BACKGROUND_COLOR = 0xEEFFFFFF;
	private static final int FOREGORUND_COLOR = 0x00000000;

	private enum ButtonCode { PREVIOUS, NEXT, CLOSE };

	private RichContent content;
	private int currentPage;
	private boolean dismissible;
	private boolean shouldPauseGame;
	private String closeButtonLabel;
	private GuiButton buttonPrevious;
	private GuiButton buttonNext;
	private GuiButton closeButton;
	private ArrayList<ScreenListener> listeners;

	/**
	 * Constructor
	 * @param content - Content of the screen
	 * @param shouldPauseGame - Indicates whether the game should be pause or not
	 * @param dismissible - Indicates whether there is a button to close the window
	 * @param closeButtonLabel - Label of the button which closes the screen
	 */
	public RichContentScreen(RichContent content, boolean shouldPauseGame, boolean dismissable, String closeButtonLabel) {
		this.content = content;
		this.currentPage = 0;
		this.dismissible = dismissable;
		this.closeButtonLabel = closeButtonLabel;
		this.listeners = new ArrayList<ScreenListener>();
		this.shouldPauseGame = shouldPauseGame;
	}

	/**
	 * Constructor
	 * @param content - Content of the screen
	 * @param shouldPauseGame - Indicates whether the game should be pause or not
	 * @param dismissible - Indicates whether there is a button to close the window
	 */
	public RichContentScreen(RichContent content, boolean shouldPauseGame, boolean dismissable) {
		this.content = content;
		this.currentPage = 0;
		this.dismissible = dismissable;		
		this.listeners = new ArrayList<ScreenListener>();
		this.shouldPauseGame = shouldPauseGame;
	}


	@Override
	public void initGui() {
		super.initGui();	
		this.drawButtons();		
	}

	/**
	 * Draw buttons on the screen
	 */
	protected void drawButtons() {
		int avaliableWidth = this.width - 2*MARGIN;
		int avaliableSpace = avaliableWidth - 3*BUTTON_WIDTH;
		int spaceBetweenButtons = avaliableSpace / 4;
		int x = MARGIN + spaceBetweenButtons;		
		int y = this.height - 2*MARGIN - BUTTON_HEIGHT;
		
		this.buttonPrevious = new GuiButton(ButtonCode.PREVIOUS.ordinal(), x, y, BUTTON_WIDTH, BUTTON_HEIGHT, "Prev");
		x = x + BUTTON_WIDTH + spaceBetweenButtons;
		this.closeButton = new GuiButton(ButtonCode.CLOSE.ordinal(), x, y, BUTTON_WIDTH, BUTTON_HEIGHT, this.closeButtonLabel);
		x = x + BUTTON_WIDTH + spaceBetweenButtons;
		this.buttonNext = new GuiButton(ButtonCode.NEXT.ordinal(), x, y, BUTTON_WIDTH, BUTTON_HEIGHT, "Next");
					
		if (this.content.getNumberOfPages() > 1) {
			this.buttonList.add(this.buttonPrevious);
			this.buttonList.add(this.buttonNext);
		}
		if (this.dismissible) {
			this.buttonList.add(this.closeButton);
		}
		this.handleButtonsActivation();
	}

	@Override
	public void drawScreen(int mouseX, int mouseY, float partialTicks) {
		this.drawBackground();
		this.drawCurrentPage();
		super.drawScreen(mouseX, mouseY, partialTicks);
	}

	/**
	 * Draw screen background
	 */
	private void drawBackground() {
		drawRect(MARGIN, MARGIN, this.width - MARGIN, this.height - MARGIN, BACKGROUND_COLOR);
	}

	/**
	 * Draw contents of the current page
	 */
	private void drawCurrentPage() {		
		RichContentPage page = this.content.getPage(this.currentPage);
		this.drawTextFromPage(page);
		this.drawImagesFromPage(page);
	}

	/**
	 * Draw textual content of a page
	 * @param page - Rich content page
	 */
	private void drawTextFromPage(RichContentPage page) {
		this.labelList.clear();

		int textWidth = this.width - 2*(MARGIN + TEXT_PADDING);
		int textHeight = TEXT_HEIGHT;
		int x = MARGIN + TEXT_PADDING;
		int y = MARGIN + TEXT_PADDING;

		GuiLabel guiLabel = new UnshadowedLabel(this.fontRendererObj, 1, x, y, textWidth, textHeight, FOREGORUND_COLOR);
		guiLabel.setCentered();

		Iterator<String> text = page.getText();
		while(text.hasNext()) {
			guiLabel.addLine(text.next());
		}
		this.labelList.add(guiLabel);
	}

	/**
	 * Draw image content of a page
	 * @param page - Rich content page
	 */
	private void drawImagesFromPage(RichContentPage page) {			
		int avaliableWidth = this.width - 2*MARGIN;
		int avaliableSpace = avaliableWidth - page.getSumOfImagesWidth();
		int spaceBetweenImages = avaliableSpace / (page.getNumberOfImages() + 1); 
		int x = MARGIN + spaceBetweenImages;
		int y = TEXT_HEIGHT + 2*MARGIN;
		int maxImageHeight = page.getMaxImageHeight();

		Iterator<RichContentImage> images = page.getImages();
		while(images.hasNext()) {
			RichContentImage image = images.next();
			this.mc.getTextureManager().bindTexture(image.getResourceLocation());
			this.drawTexturedModalRect(x, y + (maxImageHeight - image.getHeight()) / 2, image.getOriginXInTexture(), image.getOriginYInTexture(), image.getWidth(), image.getHeight());
			this.drawImageCaption(image, x, y, maxImageHeight);
			x += image.getWidth() + spaceBetweenImages;
		}
	}

	/**
	 * Draw image caption
	 * @param image - Rich content image
	 * @param imageX - Position of the image in the axis X
	 * @param imageY - Position of the image in the axis Y
	 * @param maxImageHeightOnPage - Max height among all the images in the page
	 */
	private void drawImageCaption(RichContentImage image, int imageX, int imageY, int maxImageHeightOnPage) {
		int x = imageX - (image.getWidth() / 2); 
		int y = imageY + maxImageHeightOnPage + CAPTION_VERTICAL_SPACING;
		GuiLabel guiLabel = new UnshadowedLabel(this.fontRendererObj, 1, x, y, 2*image.getWidth(), CAPTION_HEIGHT, FOREGORUND_COLOR);
		guiLabel.setCentered();
		guiLabel.addLine(image.getCaption());
		this.labelList.add(guiLabel);
	}

	@Override
	protected void actionPerformed(GuiButton guiButton) {
		ButtonCode code = ButtonCode.values()[guiButton.id];
		
		switch (code) {		
		case PREVIOUS:
			this.currentPage = Math.max(0, this.currentPage - 1);
			this.handleButtonsActivation();
			break;

		case NEXT:
			this.currentPage = Math.min(this.content.getNumberOfPages() - 1, this.currentPage + 1);
			this.handleButtonsActivation();
			break;

		case CLOSE:			
			this.dismissScreen();
			break;
		}			
	}

	/**
	 * Enable/disable buttons according to the current page
	 */
	private void handleButtonsActivation() {
		this.buttonPrevious.enabled = true;
		this.buttonNext.enabled = true;
		this.closeButton.visible = false;
		
		if (this.currentPage == 0) {
			this.buttonPrevious.enabled = false;				
		}	
		if (this.currentPage == this.content.getNumberOfPages() - 1) {
			this.buttonNext.enabled = false;							
			this.closeButton.visible = true;
		}
	}

	@Override
	public boolean doesGuiPauseGame() {
		return this.shouldPauseGame;
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
	public void dismissScreen() {
		this.mc.player.closeScreen();
		for (ScreenListener listener : this.listeners) {
			listener.screenDismissed(this);
		}		
	}	

}
