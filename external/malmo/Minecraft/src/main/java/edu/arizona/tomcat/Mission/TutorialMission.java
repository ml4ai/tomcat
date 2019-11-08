package edu.arizona.tomcat.Mission;

import com.microsoft.Malmo.Schemas.BlockType;
import com.microsoft.Malmo.Schemas.EntityTypes;
import com.microsoft.Malmo.Schemas.ItemType;

import edu.arizona.tomcat.Mission.Goal.*;
import edu.arizona.tomcat.Utils.InventoryHandler;

import net.minecraft.world.World;

public class TutorialMission extends Mission {
	
	private boolean drawn;
	
	public TutorialMission() {
		super();
		this.drawn = false; 
	}

	@Override
	protected void createPhases() {
		this.openInventory();
		this.craftAWoodAxe();
		this.goToTheMainEntrance();
		this.killAZombie();
	}
	
	private void openInventory() {
		MissionPhase openInventoryPhase = new MissionPhase();
		MissionGoal goal = new OpenInventoryGoal();		
		openInventoryPhase.addInstructionsLine("Open the inventory by typing the key E.");
		openInventoryPhase.addGoal(goal);
		this.addPhase(openInventoryPhase);
	}

	private void craftAWoodAxe() {
		MissionPhase openInventoryPhase = new MissionPhase();
		MissionGoal goal = new CraftItemGoal(ItemType.WOODEN_AXE);
		openInventoryPhase.addInstructionsLine("Craft a wood Axe in your Inventory");
		openInventoryPhase.addInstructionsLine("1. Creating the crafting table by left clicking " +
			"wooden planks and right clicking to place four wooden palcks to each grid of 2x2 crafting grids " +
			"under the 'Crafting' respectively, then hit the grid that the arrow points at to create a crafting table");
		openInventoryPhase.addInstructionsLine("2. go into your inventory and put the crafting table " +
				"in your hotbar and exit your inventory");
		openInventoryPhase.addInstructionsLine("3. Select the crafting table in the hotbar and right " +
				"click an empty area to place the crafting table. Once it is placed, walk up to it and right click on" +
				" it and it will open up its crafting menu");
		openInventoryPhase.addInstructionsLine("4. Place 3 wood planks and 2 sticks in the 3x3 " +
				"crafting grid. 2 wood planks placed in the first row. 1 wood plank and 1 stick in the second box of " +
				"second row. In third row, 1 stick in the middle box");
		openInventoryPhase.addGoal(goal);
		this.addPhase(openInventoryPhase);
	}
	
	private void goToTheMainEntrance() {
		MissionPhase goToTheMainEntrancePhase = new MissionPhase();
		MissionGoal goal = new ReachPositionGoal(0, 2, 30, 2);
		goToTheMainEntrancePhase.addInstructionsLine("Go to the main entrance of the building.");
		goToTheMainEntrancePhase.addInstructionsLine("Press the key A to move left.");
		goToTheMainEntrancePhase.addInstructionsLine("Press the key W to move forward.");
		goToTheMainEntrancePhase.addInstructionsLine("Press the key D to move right.");
		goToTheMainEntrancePhase.addInstructionsLine("Press the key S to move backwards.");	
		goToTheMainEntrancePhase.addGoal(goal);	
		this.addPhase(goToTheMainEntrancePhase);
	}
	
	private void killAZombie()  {
		MissionPhase fightMonster = new MissionPhase();
		MissionGoal goal = new KillEntityGoal();		
		fightMonster.addInstructionsLine("Battle one monster. (behind you)");
		fightMonster.addGoal(goal);
		this.addPhase(fightMonster);
	
	}

	@Override
	public void init(World world) {
		// Tutorial world can be built here if does not come from an XML
	}

	@Override
	protected void updateScene(World world) {
		// Not needed. The tutorial will load a predefined world.	
		if (!this.drawn) {
			try {
				this.drawn = true;
				InventoryHandler.addItemToInventory(ItemType.STICK, 2);
				InventoryHandler.addBlockToInventory(BlockType.PLANKS, 7);
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

}
