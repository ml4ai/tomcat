package edu.arizona.tomcat.Mission.Client;

import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessage;
import edu.arizona.tomcat.Mission.USARSinglePlayerMission;
import net.minecraft.client.Minecraft;
import net.minecraft.init.SoundEvents;
import net.minecraftforge.common.MinecraftForge;

public class USARSinglePlayerClientMission extends ClientMission {

  public USARSinglePlayerClientMission() { super(); }

  @Override
  public void handleMessageFromServer(TomcatMessage message) {
    super.handleMessageFromServer(message);
  }
}
