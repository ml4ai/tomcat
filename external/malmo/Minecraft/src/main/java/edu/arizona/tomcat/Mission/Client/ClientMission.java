package edu.arizona.tomcat.Mission.Client;

import com.microsoft.Malmo.MalmoMod;

import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessage;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessageType;
import edu.arizona.tomcat.Mission.gui.MessageScreen;
import edu.arizona.tomcat.Mission.gui.RichContentScreen;
import edu.arizona.tomcat.Mission.gui.ScreenListener;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;

public abstract class ClientMission implements ScreenListener {
	
	/**
	 * Handle message from the server side
	 */
	public void handleMessageFromServer(TomcatMessage message) {		
		switch (message.getMessageType()) {
		case SHOW_INSTRUCTIONS_SCREEN:
			RichContentScreen instructionsScreen = new RichContentScreen(message.getMessageData().getRichContent(), true, "Ok");
			instructionsScreen.addListener(this);
			Minecraft.getMinecraft().displayGuiScreen(instructionsScreen);			
			break;
		
		case SHOW_MESSAGE_SCREEN:				
			MessageScreen messageScreen = new MessageScreen(message.getMessageData().getMissionPhaseMessage());			
			Minecraft.getMinecraft().displayGuiScreen(messageScreen);
			break;
			
		case SHOW_COMPLETION_SCREEN:
			Minecraft.getMinecraft().displayGuiScreen(new RichContentScreen(message.getMessageData().getRichContent(), false));		
			break;	
		
		case DISMISS_OPEN_SCREEN:
			Minecraft.getMinecraft().player.closeScreen();
			MalmoMod.network.sendToServer(new TomcatMessage(TomcatMessageType.OPEN_SCREEN_DISMISSED));
			break;	

		default:
			break;
		}		
	};

	@Override
	public void screenDismissed(GuiScreen screen) {		
		MalmoMod.network.sendToServer(new TomcatMessage(TomcatMessageType.OPEN_SCREEN_DISMISSED));		 			
	}

}
