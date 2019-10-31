package edu.arizona.tomcat.Mission.Goal;

import net.minecraft.client.Minecraft;
import net.minecraft.entity.Entity;
import net.minecraft.entity.monster.EntityZombie;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.world.World;

import java.util.Iterator;
import java.util.List;

public class CraftItemGoal extends  MissionGoal {
    private static ItemStack itemStack;

    /* constructor to recieve Itemstack from libraries, check the player's inventory, */

    /* update check whether the item is contained */

    // check the type of Item

    public CraftItemGoal(ItemStack itemSt){
        this.itemStack =  itemSt;
    }

    @Override
    public void update(World world)
    {
        if (!this.goalAchieved) {
            EntityPlayer player = world.getPlayerEntityByName(Minecraft.getMinecraft().player.getName());
            Iterator itemStacks = player.getHeldEquipment().iterator();
            int count = 0;
            while(itemStacks.hasNext()) {
                ItemStack pItemSt = (ItemStack)itemStacks.next();
                if (itemStacks.equals(pItemSt)) {
                    hasBeenAchieved();
                }
                System.out.println("Item(Stack) " + pItemSt.getDisplayName() + " is contained");
                count++;
            }
            System.out.println("Totally " + count + " items(Stack) in players inventory");
        }

    }

}
