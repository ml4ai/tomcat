package edu.arizona.tomcat.Mission.gui;

import java.util.ArrayList;

import edu.arizona.tomcat.Utils.Converter;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiLabel;

public class MessageScreen extends GUIScreenUndismissableOnEscapeKey {
	
	private String message;
	private int secondsUntilDismiss;
	private ArrayList<ScreenListener> listeners;
	private long initialTime;
	
	public MessageScreen(String message, int secondsUntilDismiss) {
		this.message = message;
		this.secondsUntilDismiss = secondsUntilDismiss;
		this.listeners = new ArrayList<ScreenListener>();
	}
	
	@Override
	public void initGui() {
		super.initGui();	
		
		GuiLabel messageLabel = new GuiLabel(fontRendererObj, 1, (this.width - 200) / 2, this.height / 4, 200, 20, 0xFFFFFF);
		messageLabel.addLine(this.message);
		messageLabel.setCentered();		
		this.labelList.add(messageLabel);
	}

	@Override
	public boolean doesGuiPauseGame() {
		return false;
	}
	
	/**
	 * Updates the screen to decide if it must be dismissed
	 */
	public void update() {
		int remainingSeconds = this.getRemainingTimeToDismiss();
		
		if (remainingSeconds <= 0) {
			this.dismissScreen();
		}
	}
	
	/**
	 * Get remaining time to dismiss the screen
	 * @return
	 */
	private int getRemainingTimeToDismiss() {
		long currentWorldTime = Minecraft.getMinecraft().world.getTotalWorldTime();
		
		if (this.initialTime == 0) {
			this.initialTime = currentWorldTime;
		}	
		
		return Converter.getRemainingTimeInSeconds(this.initialTime, this.secondsUntilDismiss);
	}
	
	/**
	 * Adds listener to be notified once the screen is closed
	 * @param listener - Object that wants to be notified when the screen closes
	 */
	public void addListener(ScreenListener listener) {
		this.listeners.add(listener);
	}
	
	/**
	 * Close the GUI and notify listeners about it
	 */
	public void dismissScreen() {
		this.mc.player.closeScreen();
		for (ScreenListener listener : this.listeners) {
			listener.screenDismissed(this, null);
		}		
	}

}
