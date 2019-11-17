package edu.arizona.tomcat.Mission.Client;

import java.util.List;

import com.microsoft.Malmo.MalmoMod;

import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessage;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessageType;
import edu.arizona.tomcat.Mission.gui.InstructionsScreen;
import edu.arizona.tomcat.Mission.gui.MessageScreen;
import edu.arizona.tomcat.Mission.gui.ScreenListener;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;
import scala.actors.threadpool.Arrays;

public abstract class ClientMission implements ScreenListener {
	
	/**
	 * Handle message from the server side
	 */
	public void handleMessageFromServer(TomcatMessage message) {		
		switch (message.getMessageType()) {
		case SHOW_INSTRUCTIONS_SCREEN:
			@SuppressWarnings("unchecked") List<String> instructions = Arrays.asList(message.getMessageData().getMissionPhaseInstructions().split("\n"));
			InstructionsScreen instructionsScreen = new InstructionsScreen(instructions);
			instructionsScreen.addListener(this);
			Minecraft.getMinecraft().displayGuiScreen(instructionsScreen);
			break;
		
		case SHOW_MESSAGE_SCREEN:				
			@SuppressWarnings("unchecked") List<String> messageScreenText = Arrays.asList(message.getMessageData().getMissionPhaseMessage().split("\n"));
			MessageScreen messageScreen = new MessageScreen(messageScreenText);			
			Minecraft.getMinecraft().displayGuiScreen(messageScreen);
			break;
			
		case SHOW_COMPLETION_SCREEN:
			Minecraft.getMinecraft().displayGuiScreen(this.getCompletionScreenInstance());		
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
		if (screen instanceof InstructionsScreen) {
			MalmoMod.network.sendToServer(new TomcatMessage(TomcatMessageType.INSTRUCTIONS_SCREEN_DISMISSED));
		} 			
	}

	/**
	 * Retrieves a new instance of the screen that should be opened on the completion of a mission
	 * @return
	 */
	public abstract GuiScreen getCompletionScreenInstance();

}
