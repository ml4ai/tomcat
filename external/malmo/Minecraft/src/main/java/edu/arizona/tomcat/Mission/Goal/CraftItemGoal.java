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

public class CraftItemGoal extends  MissionGoal {
    private static ItemType type;

    /* constructor to recieve Itemstack from libraries, check the player's inventory, */

    /* update check whether the item is contained */

    // check the type of Item

    public CraftItemGoal(ItemType ITy){
        this.type =  ITy;
    }

    @Override
    public void update(World world)
    {
        if (!this.goalAchieved) {

            if(InventoryHandler.checkItemToInventory(type, 1)){
                this.goalAchieved = true;
            }

             System.out.println("Item(Stack) " + type.name() + " is contained");

        }

    }

}
