package edu.arizona.tomcat.Mission.Goal;

import com.microsoft.Malmo.Schemas.ItemType;
import edu.arizona.tomcat.Utils.InventoryHandler;
import edu.arizona.tomcat.Utils.MinecraftServerHelper;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.world.World;

public class CraftItemGoal extends MissionGoal {
    private ItemType itemType;

    /**
     * Constructor
     * @param itemType - Type of the item to be checked in the inventory
     */
    public CraftItemGoal(ItemType itemType) { this.itemType = itemType; }

    @Override
    public void updateGoalStatus(World world) {
        for (EntityPlayerMP player :
             MinecraftServerHelper.getServer().getPlayerList().getPlayers()) {
            this.goalAchieved =
                InventoryHandler.checkItemToInventory(player, this.itemType, 1);

            if (this.goalAchieved) {
                this.player = player;
                break;
            }
        }
    }
}
