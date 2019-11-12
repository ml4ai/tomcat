package edu.arizona.tomcat.Mission.Client;

import com.microsoft.Malmo.MalmoMod;

import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessage;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessageType;
import edu.arizona.tomcat.Mission.SARMission;
import edu.arizona.tomcat.Mission.gui.GUIOverlayVillagersSaved;
import edu.arizona.tomcat.Mission.gui.InstructionsScreen;
import edu.arizona.tomcat.Mission.gui.SARCompletionScreen;
import edu.arizona.tomcat.Mission.gui.ScreenListener;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;
import net.minecraftforge.common.MinecraftForge;
import scala.actors.threadpool.Arrays;

public class SARClientMission extends ClientMission implements ScreenListener {
	
	private int numberOfSavedVillagers;
	
	public SARClientMission() {
		// Sets indicator of number of villagers saved to be rerendered by a Minecraft event schema.
		MinecraftForge.EVENT_BUS.register(new GUIOverlayVillagersSaved(SARMission.NUMBER_OF_VILLAGERS));
	}

	@Override
	public void handleMessageFromServer(TomcatMessage message) {
		switch (message.getMessageType()) {
		case SHOW_COMPLETION_SCREEN:
			SARCompletionScreen completionScreen = new SARCompletionScreen(this.numberOfSavedVillagers);
			Minecraft.getMinecraft().displayGuiScreen(completionScreen);		
			break;
			
		case VILLAGER_SAVED:
			this.numberOfSavedVillagers++;
			break;
		
		case SHOW_INSTRUCTIONS_SCREEN:
			InstructionsScreen instructionsScreen = new InstructionsScreen(Arrays.asList(message.getMessageData().getMissionPhaseInstructions().split("\n")));
			instructionsScreen.addListener(this);
			Minecraft.getMinecraft().displayGuiScreen(instructionsScreen);
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
	public void screenDismissed(GuiScreen screen, ButtonType buttonType) {
		MalmoMod.network.sendToServer(new TomcatMessage(TomcatMessageType.INSTRUCTIONS_SCREEN_DISMISSED));		
	}
		
}
