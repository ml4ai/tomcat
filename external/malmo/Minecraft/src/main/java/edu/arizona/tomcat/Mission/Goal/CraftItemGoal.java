package edu.arizona.tomcat.Mission.Goal;

import com.microsoft.Malmo.Schemas.ItemType;
import edu.arizona.tomcat.Utils.InventoryHandler;
import net.minecraft.world.World;

/**
 *  CraftItemGoal is a class to check whehther a specific type of Item received from constructor exists in player's
 *  inventory
 */

public class CraftItemGoal extends  MissionGoal {
    private static ItemType type;  // itemtype that needs to check

    /*
    * Constructor to receive Itemstack from libraries, check the player's inventory,
    * @param ITy: itemtype that needs to check
    */
    public CraftItemGoal(ItemType ITy){
        this.type =  ITy;
    }

    /*
    * Update to check whether the item is contained
    * @param world: World object that contains the player
     */
    @Override
    public void update(World world)
    {
        if (!this.goalAchieved) {
            if(InventoryHandler.checkItemToInventory(type, 1)){
                this.goalAchieved = true;
            }
        }
    }

}
