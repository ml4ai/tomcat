package edu.arizona.tomcat.Mission.Client;

import com.microsoft.Malmo.MalmoMod;

import edu.arizona.tomcat.Messaging.TomcatMessageData;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessage;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessageType;
import edu.arizona.tomcat.Mission.gui.MessageScreen;
import edu.arizona.tomcat.Mission.gui.RichContent;
import edu.arizona.tomcat.Mission.gui.RichContentScreen;
import edu.arizona.tomcat.Mission.gui.ScreenListener;
import edu.arizona.tomcat.Mission.gui.SelfReportContent;
import edu.arizona.tomcat.Mission.gui.SelfReportScreen;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;

public abstract class ClientMission implements ScreenListener {
	
	/**
	 * Handle message from the server side
	 */
	public void handleMessageFromServer(TomcatMessage message) {		
		switch (message.getMessageType()) {
		case SHOW_INSTRUCTIONS_SCREEN:
			RichContentScreen instructionsScreen = new RichContentScreen(message.getMessageData().getRichContent(), true, true, "Ok");
			instructionsScreen.addListener(this);
			Minecraft.getMinecraft().displayGuiScreen(instructionsScreen);			
			break;
		
		case SHOW_MESSAGE_SCREEN:				
			MessageScreen messageScreen = new MessageScreen(message.getMessageData().getMissionPhaseMessage());			
			Minecraft.getMinecraft().displayGuiScreen(messageScreen);
			break;
			
		case SHOW_COMPLETION_SCREEN:
			Minecraft.getMinecraft().displayGuiScreen(new RichContentScreen(message.getMessageData().getRichContent(), false, false));		
			break;	
		
		case DISMISS_OPEN_SCREEN:
			Minecraft.getMinecraft().player.closeScreen();
			MalmoMod.network.sendToServer(new TomcatMessage(TomcatMessageType.OPEN_SCREEN_DISMISSED));
			break;	
		
		case SHOW_SELF_REPORT:
			SelfReportScreen selfReportScreen = new SelfReportScreen(message.getMessageData().getSelfReport(), true);
			selfReportScreen.addListener(this);
			Minecraft.getMinecraft().displayGuiScreen(selfReportScreen);			
			break;

		default:
			break;
		}		
	};

	@Override
	public void screenDismissed(GuiScreen screen) {		
		MalmoMod.network.sendToServer(new TomcatMessage(TomcatMessageType.OPEN_SCREEN_DISMISSED));		 			
	}
	
	@Override
	public void screenDismissed(GuiScreen screen, SelfReportContent selfReport) {
		TomcatMessageData data = new TomcatMessageData(selfReport);
		data.setPlayerName(Minecraft.getMinecraft().player.getName());
		MalmoMod.network.sendToServer(new TomcatMessage(TomcatMessageType.SELF_REPORT_ANSWERED, data));		
	}
	
	/**
	 * Show screen informing the user connection was lost with malmo client
	 */
	public void showScreenConnectionLost() {
		RichContent content = RichContent.createFromJson("connection_lost.json");
		Minecraft.getMinecraft().displayGuiScreen(new RichContentScreen(content, false, false));
	}

}
