package edu.arizona.tomcat.Mission.Client;

import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessage;
import edu.arizona.tomcat.Mission.TutorialMission;
import edu.arizona.tomcat.Mission.gui.GUIOverlayVillagersSaved;
import net.minecraft.client.Minecraft;
import net.minecraftforge.common.MinecraftForge;

public class TutorialClientMission extends ClientMission {

	private int numberOfSavedVillagers;

	public TutorialClientMission() {
		// Sets indicator of number of villagers saved to be rerendered by a Minecraft event schema.
		MinecraftForge.EVENT_BUS.register(new GUIOverlayVillagersSaved(TutorialMission.NUMBER_OF_VILLAGERS));
	}

	@Override
	public void handleMessageFromServer(TomcatMessage message) {
		super.handleMessageFromServer(message);

		switch (message.getMessageType()) {
		case VILLAGER_SAVED:
			this.numberOfSavedVillagers++;
			break;

			case VIEW_CHANGED:
			if (Minecraft.getMinecraft().gameSettings.thirdPersonView < 2) {
				Minecraft.getMinecraft().gameSettings.thirdPersonView++;
			}
			else {
				Minecraft.getMinecraft().gameSettings.thirdPersonView = 0;
			}
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

}
