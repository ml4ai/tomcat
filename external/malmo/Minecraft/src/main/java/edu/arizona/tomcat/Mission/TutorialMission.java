package edu.arizona.tomcat.Mission;

import com.microsoft.Malmo.Schemas.BlockType;
import com.microsoft.Malmo.Schemas.ItemType;

import edu.arizona.tomcat.Mission.Goal.MissionGoal;
import edu.arizona.tomcat.Mission.Goal.OpenInventoryGoal;
import edu.arizona.tomcat.Mission.Goal.ReachPositionGoal;
import edu.arizona.tomcat.Utils.InventoryHandler;
import edu.arizona.tomcat.World.Drawing;
import edu.arizona.tomcat.World.Room;
import net.minecraft.init.Blocks;
import net.minecraft.item.ItemDoor;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.Explosion;
import net.minecraft.world.World;

public class TutorialMission extends Mission {

	private boolean drawn;
	private boolean blockRoom3;
	private MissionPhase approachRoom3Phase;

	public TutorialMission() {
		super();
		this.drawn = false; 
		this.blockRoom3 = false;
	}

	@Override
	protected void createPhases() {		
		//		this.openInventory();
		//		this.craftAWoodAxe();
		//		this.goToTheMainEntrance();
		//		this.killAZombie();		
		this.approachRoom3Phase = new MissionPhase();
		this.approachRoom3Phase.addGoal(new ReachPositionGoal(-11, 2, 2, 1));
		this.addPhase(this.approachRoom3Phase);
	}

	private void openInventory() {
		MissionPhase openInventoryPhase = new MissionPhase();
		MissionGoal goal = new OpenInventoryGoal();		
		openInventoryPhase.addInstructionsLine("Open the inventory by typing the key E.");
		openInventoryPhase.setShowCompletionMessage(true);
		openInventoryPhase.addGoal(goal);
		this.addPhase(openInventoryPhase);
	}

	private void craftAWoodAxe() {
		// TODO Auto-generated method stub		
	}

	private void goToTheMainEntrance() {
		MissionPhase goToTheMainEntrancePhase = new MissionPhase();
		MissionGoal goal = new ReachPositionGoal(0, 2, 30, 2);
		goToTheMainEntrancePhase.addInstructionsLine("Go to the main entrance of the building.");
		goToTheMainEntrancePhase.addInstructionsLine("Press the key A to move left.");
		goToTheMainEntrancePhase.addInstructionsLine("Press the key W to move forward.");
		goToTheMainEntrancePhase.addInstructionsLine("Press the key D to move right.");
		goToTheMainEntrancePhase.addInstructionsLine("Press the key S to move backwards.");	
		goToTheMainEntrancePhase.setShowCompletionMessage(true);
		goToTheMainEntrancePhase.addGoal(goal);	
		this.addPhase(goToTheMainEntrancePhase);
	}

	private void killAZombie() {
		// TODO Auto-generated method stub		
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
				//this.placeTNTOnMainEntrance(world);
				this.createCollapsableBuilding(world);
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}		

		}		

		if (this.blockRoom3) {
			this.explodeDoorOfRoom3(world);
		}

	}

	private void explodeDoorOfRoom3(World world) {
		Explosion explosion = world.createExplosion(null, -17, 2, 2, 1, true);
		explosion.doExplosionA();
		explosion.doExplosionB(true);
		this.blockRoom3 = false;
	}

	private void createCollapsableBuilding(World world) throws Exception {
		Drawing drawing = new Drawing();
		Room groundFloor = new Room(-10, 2, 0, 10, 6, 10, BlockType.SAND, false);
		Room room1 = new Room(-10, 2, 6, 4, 6, 4, BlockType.SAND, false);
		Room room2 = new Room(-15, 2, 6, 5, 6, 4, BlockType.SAND, false);
		Room room3 = new Room(-17, 2, 0, 3, 6, 5, BlockType.SAND, false);
		drawing.addObject(groundFloor);
		drawing.addObject(room1);
		drawing.addObject(room2);
		drawing.addObject(room3);
		this.drawingHandler.draw(world, drawing);
		ItemDoor.placeDoor(world, new BlockPos(-10,2,2), EnumFacing.WEST, Blocks.OAK_DOOR, true);
		ItemDoor.placeDoor(world, new BlockPos(-12,2,6), EnumFacing.SOUTH, Blocks.OAK_DOOR, true);
		ItemDoor.placeDoor(world, new BlockPos(-16,2,6), EnumFacing.SOUTH, Blocks.OAK_DOOR, true);
		ItemDoor.placeDoor(world, new BlockPos(-17,2,2), EnumFacing.WEST, Blocks.OAK_DOOR, true);		
	}

	@Override
	public void phaseCompleted() {
		if (this.currentPhase.equals(this.approachRoom3Phase)) {
			this.blockRoom3 = true;
		}
		super.phaseCompleted();
	}

}
