package edu.arizona.tomcat.Mission.Client;

import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessage;
import edu.arizona.tomcat.Mission.SARMission;
import edu.arizona.tomcat.Mission.gui.GUIOverlayVillagersSaved;
import edu.arizona.tomcat.Mission.gui.SARCompletionScreen;
import net.minecraft.client.gui.GuiScreen;
import net.minecraftforge.common.MinecraftForge;

public class SARClientMission extends ClientMission {

	private int numberOfSavedVillagers;

	public SARClientMission() {
		// Sets indicator of number of villagers saved to be rerendered by a Minecraft event schema.
		MinecraftForge.EVENT_BUS.register(new GUIOverlayVillagersSaved(SARMission.NUMBER_OF_VILLAGERS));
	}

	@Override
	public void handleMessageFromServer(TomcatMessage message) {
		super.handleMessageFromServer(message);

		switch (message.getMessageType()) {
		case VILLAGER_SAVED:
			this.numberOfSavedVillagers++;
			break;		

		default:
			break;
		}		
	}	

	/**
	 * Retrieves the total number of villagers saved by the player
	 * @param numberOfSavedVillagers - Total number of rescued villagers
	 */
	public int getNumberOfSavedVillagers() {
		return this.numberOfSavedVillagers;
	}

	@Override
	public GuiScreen getCompletionScreenInstance() {
		return new SARCompletionScreen(this.numberOfSavedVillagers);
	}


}
