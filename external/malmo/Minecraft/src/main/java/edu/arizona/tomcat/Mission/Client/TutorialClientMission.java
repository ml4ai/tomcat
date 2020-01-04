package edu.arizona.tomcat.Mission.Client;

import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessage;
import edu.arizona.tomcat.Mission.TutorialMission;
import edu.arizona.tomcat.Mission.gui.GUIOverlayVillagersSaved;
import net.minecraft.client.Minecraft;
import net.minecraftforge.common.MinecraftForge;

public class TutorialClientMission extends ClientMission {

	private int numberOfSavedVillagers;
	private enum CAMERA_PERSPECTIVE {FIRST_PERSON, BACK_VIEW, FRONT_VIEW};

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
			this.showNextCameraPerspective();
			break;
		default:
			break;
		}		
	}	

	/**
	 * Retrieves the total number of villagers saved by the player
	 */
	public int getNumberOfSavedVillagers() {
		return this.numberOfSavedVillagers;
	}
	
	/**
	 * Changes the camera focus to the next perspective 
	 */
	private void showNextCameraPerspective() {
		CAMERA_PERSPECTIVE currentCameraPerspective = CAMERA_PERSPECTIVE.values()[Minecraft.getMinecraft().gameSettings.thirdPersonView];
		CAMERA_PERSPECTIVE nextCameraPerspective = currentCameraPerspective;
		
		switch (currentCameraPerspective) {
		case FIRST_PERSON:
			nextCameraPerspective = CAMERA_PERSPECTIVE.BACK_VIEW;
			break;
		
		case BACK_VIEW:
			nextCameraPerspective = CAMERA_PERSPECTIVE.FRONT_VIEW;
			break;
			
		case FRONT_VIEW:
			nextCameraPerspective = CAMERA_PERSPECTIVE.FIRST_PERSON;
			break;

		default:
			break;
		}
		
		Minecraft.getMinecraft().gameSettings.thirdPersonView = nextCameraPerspective.ordinal();
	}
}
