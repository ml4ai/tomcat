package edu.arizona.tomcat.Messaging;

import com.microsoft.Malmo.MalmoMod;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessageType;
import edu.arizona.tomcat.Mission.Client.ClientMission;
import edu.arizona.tomcat.Mission.MissionFactory;
import edu.arizona.tomcat.Utils.MinecraftServerHelper;
import java.util.HashSet;
import java.util.Set;
import net.minecraft.entity.player.EntityPlayerMP;

public class TomcatClientServerHandler {

  private static Set<String> pendingPlayers = new HashSet<String>();

  /**
   * Send a message to all the clients that are not in a pending state
   * @param message - Message to be sent
   */
  public static void
  sendMessageToAllClients(TomcatMessaging.TomcatMessage message,
                          boolean expectReply) {
    for (EntityPlayerMP player :
         MinecraftServerHelper.getServer().getPlayerList().getPlayers()) {
      sendMessageToClient(player, message, expectReply);
    }
  }

  /**
   * Send a message to a specific client
   * @param message - Message to be sent
   */
  public static void sendMessageToClient(EntityPlayerMP player,
                                         TomcatMessaging.TomcatMessage message,
                                         boolean expectReply) {
    if ((!isTomcat(player) || !isMultiplayer()) &&
        !pendingPlayers.contains(player.getName())) {
      if (expectReply) {
        pendingPlayers.add(player.getName());
        MalmoMod.instance.getServer().getTomcatServerMission().pauseMission();
      }
      MalmoMod.network.sendTo(message, player);
    }
  }

  /**
   * Send a message to the server
   * @param message - Message to be sent
   */
  public static void
  sendMessageToServer(TomcatMessaging.TomcatMessage message) {
    MalmoMod.network.sendToServer(message);
  }

  /**
   * Checks if the player is TOMCAT
   * @param player - Player
   * @return
   */
  public static boolean isTomcat(EntityPlayerMP player) {
    return player.getName().equals("tomcat");
  }

  /**
   * Checks if the mission is multiplayer
   * @return
   */
  public static boolean isMultiplayer() {
    return MinecraftServerHelper.getServer()
               .getPlayerList()
               .getPlayers()
               .size() > 1;
  }

  public static void
  handleMessageBackFromClient(EntityPlayerMP player,
                              TomcatMessaging.TomcatMessage message) {
    pendingPlayers.remove(player.getName());
    MalmoMod.instance.getServer()
        .getTomcatServerMission()
        .handleMessageFromClient(player, message);
  }

  public static boolean haveAllClientsReplied() {
    return pendingPlayers.isEmpty();
  }

  public static void
  handleMessageFromServer(TomcatMessaging.TomcatMessage message) {
    if (message.getMessageType() == TomcatMessageType.INIT_MISSION) {
      ClientMission clientMission = MissionFactory.createClient(
          message.getMessageData().getMissionID().ordinal());
      MalmoMod.instance.getClient().setTomcatClientMission(clientMission);
    }
    else {
      if (MalmoMod.instance.getClient().getTomcatClientMission() != null) {
        MalmoMod.instance.getClient()
            .getTomcatClientMission()
            .handleMessageFromServer(message);
      }
    }
  }
}
