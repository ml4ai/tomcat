package edu.arizona.tomcat.Utils;

import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.server.MinecraftServer;
import net.minecraftforge.fml.common.FMLCommonHandler;

public class MinecraftServerHelper {

  /**
   * Retrieves the server. This method must be called by the server Thread
   * @return
   */
  public static MinecraftServer getServer() {
    return FMLCommonHandler.instance().getMinecraftServerInstance();
  }

  /**
   * Retireves the first player of the list of players
   * @return
   */
  public static EntityPlayerMP getFirstPlayer() {
    return getServer().getPlayerList().getPlayers().get(0);
  }
}
