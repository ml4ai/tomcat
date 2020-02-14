package edu.arizona.tomcat.Mission.gui;


import java.util.ArrayList;
import java.util.Iterator;

import net.minecraft.client.gui.GuiButton;
import net.minecraft.client.gui.GuiLabel;


public class SelfReportScreen extends GUIScreenUndismissableOnEscapeKey {

	private static final int MARGIN = 10;
	private static final int TEXT_PADDING = 10;
	private static final int TEXT_HEIGHT = 80;
	private static final int SMALL_BUTTON_WIDTH = 50;
	private static final int BUTTON_WIDTH = 100;
	private static final int BUTTON_HEIGHT = 20;
	private static final int BACKGROUND_COLOR = 0xEEFFFFFF;
	private static final int FOREGORUND_COLOR = 0x00000000;
	private static final int MAX_BUTTONS_PER_ROW = 3;
	private static final int SKIP_BUTTON_CODE = -1;
	private static final int OK_BUTTON_CODE = 0;
	private static final String SKIP_BUTTON_TEXT = "Skip";

	private SelfReportContent content;
	private int currentQuestion;
	private boolean shouldPauseGame;
	private boolean selfReportComplete;
	private boolean firstTimeSelfReport;
	private long initialTime;
	private ArrayList<ScreenListener> listeners;	 
	private ArrayList<String> preamble;
	
	/**
	 * Constructor
	 * @param content - Content of the screen
	 * @param shouldPauseGame - Indicates whether the game should be pause or not
	 */
	public SelfReportScreen(SelfReportContent content, boolean shouldPauseGame, boolean firstTimeSelfReport) {
		this.content = content;
		this.currentQuestion = 0;
		this.listeners = new ArrayList<ScreenListener>();
		this.shouldPauseGame = shouldPauseGame;
		this.selfReportComplete = false;
		this.firstTimeSelfReport = firstTimeSelfReport;		
	}

	@Override
	public void initGui() {
		super.initGui();	
		// We take the system time instead of the world time because the game is paused when the self-report screen is prompted
		this.initialTime = System.currentTimeMillis();
	}

	@Override
	public void drawScreen(int mouseX, int mouseY, float partialTicks) {
		this.drawBackground();
		this.drawPageContent();
		super.drawScreen(mouseX, mouseY, partialTicks);
	}

	/**
	 * Draws screen background
	 */
	private void drawBackground() {
		drawRect(MARGIN, MARGIN, this.width - MARGIN, this.height - MARGIN, BACKGROUND_COLOR);
	}
	
	/**
	 * Draws content of the current page (question or final words)
	 */
	private void drawPageContent() {
		if(this.selfReportComplete) {
			this.drawLastPage();
		} else {
			this.drawCurrentQuestion();
		}
	}
	
	/**
	 * Draws contents of the last page
	 */
	private void drawLastPage() {		
		this.drawLastPageText();
		this.drawLastPageButton();
	}
	
	/**
	 * Draws the last page textual content
	 */
	private void drawLastPageText() {
		this.labelList.clear();
		GuiLabel guiLabel = this.createGuiLabel();
		Iterator<String> text = this.content.getSpeechAfterQuestions();
		while(text.hasNext()) {
			guiLabel.addLine(text.next());
		}
		this.labelList.add(guiLabel);
	}
	
	/**
	 * Creates a new GuiLabel correctly positioned in the screen
	 * @return
	 */
	private GuiLabel createGuiLabel() {
		int textWidth = this.width - 2*(MARGIN + TEXT_PADDING);
		int textHeight = TEXT_HEIGHT;
		int x = MARGIN + TEXT_PADDING;
		int y = MARGIN + TEXT_PADDING;

		GuiLabel guiLabel = new UnshadowedLabel(this.fontRendererObj, 1, x, y, textWidth, textHeight, FOREGORUND_COLOR);
		guiLabel.setCentered();
		return guiLabel;
	}
	
	/**
	 * Draws the last page button
	 */
	protected void drawLastPageButton() {
		this.buttonList.clear();
		
		int x = (this.width - SMALL_BUTTON_WIDTH)/2; 
		int y = this.height - MARGIN - BUTTON_HEIGHT - TEXT_PADDING;
		GuiButton okButton = new GuiButton(OK_BUTTON_CODE, x, y, SMALL_BUTTON_WIDTH, BUTTON_HEIGHT, "Ok");
		this.buttonList.add(okButton);			
	}
	
	/**
	 * Draws contents of the current question
	 */
	private void drawCurrentQuestion() {		
		SelfReportQuestion question = this.content.getQuestion(this.currentQuestion);
		this.drawTextFromQuestion(question);
		this.drawButtonsFromQuestion(question);
	}

	/**
	 * Draws textual content of a page
	 * @param page - Rich content page
	 */
	private void drawTextFromQuestion(SelfReportQuestion question) {
		this.labelList.clear();
		GuiLabel guiLabel = this.createGuiLabel();
		
		this.drawPreamble(guiLabel);		
		Iterator<String> text = question.getText();
		while(text.hasNext()) {
			guiLabel.addLine(text.next());
		}
		
		this.labelList.add(guiLabel);
	}
	
	/**
	 * Draws question preamble
	 */
	private void drawPreamble(GuiLabel guiLabel) {
		if(this.currentQuestion == 0) {			
			Iterator<String> preamble;
			if (this.firstTimeSelfReport) {			
				preamble = content.getFirstTimePreamble();
			} else {			
				preamble = content.getRecurrentPreamble();
			}
			while(preamble.hasNext()) {
				guiLabel.addLine(preamble.next());
			}
			guiLabel.addLine("");
		}	
	}
	
	/**
	 * Draws buttons on the screen
	 */
	private void drawButtonsFromQuestion(SelfReportQuestion question) {
		this.buttonList.clear();
		
		int numberOfButtons = question.getNumberOfChoices();
		int numberOfRows = (int) Math.ceil(numberOfButtons / MAX_BUTTONS_PER_ROW) + 1; // One more extra row for the skip button			
		int y = this.height - MARGIN -  numberOfRows*(MARGIN + BUTTON_HEIGHT);
		
		Iterator<String> buttonTexts = question.getChoices();
		ArrayList<String> buttonsInARow = new ArrayList<String>();
		int numberOfButtonsAdded = 0;
		int initialButtonCode = 1;
		while (buttonTexts.hasNext()) {
			buttonsInARow.add(buttonTexts.next());
			numberOfButtonsAdded++;
			
			if(buttonsInARow.size() == MAX_BUTTONS_PER_ROW || numberOfButtonsAdded == numberOfButtons) {
				this.drawButtonRow(initialButtonCode, y, buttonsInARow);
				buttonsInARow.clear();
				y += MARGIN + BUTTON_HEIGHT;
				initialButtonCode += buttonsInARow.size();
			}
		}
		
		// Add skip button
		buttonsInARow.add(SKIP_BUTTON_TEXT);
		this.drawButtonRow(SKIP_BUTTON_CODE, y, buttonsInARow);		
	}
	
	/**
	 * Draws horizontally aligned buttons
	 */
	private void drawButtonRow(int initialButtonCode, int y, ArrayList<String> buttonTexts) {
		int avaliableWidth = this.width - 2*MARGIN;
		int avaliableSpace = avaliableWidth - buttonTexts.size()*BUTTON_WIDTH;
		int spaceBetweenButtons = avaliableSpace / (buttonTexts.size() + 1);
		int x = MARGIN + spaceBetweenButtons;
		int buttonCode = initialButtonCode;
		
		for (int i = 0; i < buttonTexts.size(); i++) {
			String buttonText  = buttonTexts.get(i);
			GuiButton choiceButton = new GuiButton(buttonCode, x, y, BUTTON_WIDTH, BUTTON_HEIGHT, buttonText);
			this.buttonList.add(choiceButton);
			x += BUTTON_WIDTH + spaceBetweenButtons;
			buttonCode++;
		}
	}

	@Override
	protected void actionPerformed(GuiButton guiButton) {
		if(guiButton.id == OK_BUTTON_CODE) {
			this.dismissScreen();
		} else {
			SelfReportQuestion question = this.content.getQuestion(this.currentQuestion);
			question.setSelectedChoice(guiButton.id);
			
			if(this.currentQuestion == this.content.getNumberOfQuestions() - 1) {
				this.selfReportComplete = true;				
			} else {
				this.currentQuestion++;
			}
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
	 * Close the GUI and notify listeners
	 * @param buttonType - Type of the button pressed
	 */
	public void dismissScreen() {
		this.setSelfReportDuration();
		this.mc.player.closeScreen();
		for (ScreenListener listener : this.listeners) {
			listener.screenDismissed(this, this.content);
		}		
	}	
	
	/**
	 * Defines the number of seconds the player took to answer the self-report
	 */
	private void setSelfReportDuration() {
		long finalTime = System.currentTimeMillis();
		float duration = (finalTime - this.initialTime) / 1000;
		this.content.setDurationInSeconds(duration);
	}

}
