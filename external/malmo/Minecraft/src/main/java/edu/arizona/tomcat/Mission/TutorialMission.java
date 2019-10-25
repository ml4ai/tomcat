package edu.arizona.tomcat.Mission;

import com.microsoft.Malmo.Schemas.BlockType;
import com.microsoft.Malmo.Schemas.EntityTypes;
import com.microsoft.Malmo.Schemas.ItemType;

import edu.arizona.tomcat.Mission.Goal.KillEntityGoal;
import edu.arizona.tomcat.Mission.Goal.MissionGoal;
import edu.arizona.tomcat.Mission.Goal.OpenInventoryGoal;
import edu.arizona.tomcat.Mission.Goal.ReachPositionGoal;
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
		//this.craftAWoodAxe();
		this.goToTheMainEntrance();
		this.killAZombie();
		this.craftAWoodAxe();
		
				
	}
	
	private void openInventory() {
		MissionPhase openInventoryPhase = new MissionPhase();
		MissionGoal goal = new OpenInventoryGoal();		
		openInventoryPhase.addInstructionsLine("Open the inventory by typing the key E.");
		openInventoryPhase.addGoal(goal);
		this.addPhase(openInventoryPhase);
	}
	
	private void craftAWoodAxe() {
		// TODO Auto-generated method stub		
		MissionPhase openInventoryPhase = new MissionPhase();
		MissionGoal goal = new OpenInventoryGoal();		
		openInventoryPhase.addInstructionsLine("Used to check if succeed in battling.");
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
		fightMonster.addInstructionsLine("Battle one monster.");
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
//				Drawing drawing = new Drawing();
//				//Plane plane = new Plane(0, 2, 10, 4, 2, 3, BlockType.STONE);
//				Entity villager = new Entity(0, 10, 2, EntityTypes.VILLAGER);
//				Room room = new Room(0, 2, 10, 4, 2, 3, BlockType.STONE, true);
//				Item door = new Item(10, 2, 0, ItemType.WOODEN_DOOR);
//				drawing.addObject(room);
//				drawing.addObject(villager);
//				drawing.addObject(door);
//				this.drawingHandler.draw(world, drawing);
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
