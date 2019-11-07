package edu.arizona.tomcat.Mission.Goal;

import com.microsoft.Malmo.Schemas.ItemType;
import edu.arizona.tomcat.Utils.InventoryHandler;
import edu.arizona.tomcat.Utils.MinecraftServerHelper;
import net.minecraft.client.Minecraft;
import net.minecraft.entity.Entity;
import net.minecraft.entity.monster.EntityZombie;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.world.World;

import java.util.Iterator;
import java.util.List;

/**
 *  CraftItemGoal is a class to check whehther a specific type of Item received from constructor exists in plaer's
 *  inventory
 */

public class CraftItemGoal extends  MissionGoal {
    private static ItemType type;  // itemtype that needs to check

    /*
    * constructor to recieve Itemstack from libraries, check the player's inventory,
    * @param ITy: itemtype that needs to check
    */
    public CraftItemGoal(ItemType ITy){
        this.type =  ITy;
    }

    /*
    * update to check whether the item is contained
    * @param world: World object that contains the player
     */
    @Override
    public void update(World world)
    {
        if (!this.goalAchieved) {
            if(InventoryHandler.checkItemToInventory(type, 1)){
                this.goalAchieved = true;
            }
            // System.out.println("Item(Stack) " + type.name() + " is contained");
        }

    }

}
