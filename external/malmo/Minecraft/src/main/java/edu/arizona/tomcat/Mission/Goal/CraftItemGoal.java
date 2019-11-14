package edu.arizona.tomcat.Mission.Goal;

import com.microsoft.Malmo.Schemas.ItemType;

import edu.arizona.tomcat.Utils.InventoryHandler;
import net.minecraft.world.World;

public class CraftItemGoal extends  MissionGoal {
	private ItemType itemType;  

	/**
	 * Constructor 
	 * @param itemType - Type of the item to be checked in the inventory
	 */
	public CraftItemGoal(ItemType itemType){
		this.itemType =  itemType;
	}

	@Override
	public void updateGoalStatus(World world)
	{
		this.goalAchieved = InventoryHandler.checkItemToInventory(this.itemType, 1);
	}

}
