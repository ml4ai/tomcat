package edu.arizona.tomcat.Utils;

import java.util.List;

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
   * Retireves the list of players
   * @return
   */
  public static List<EntityPlayerMP> getPlayers() {
    return getServer().getPlayerList().getPlayers();
  }
}
