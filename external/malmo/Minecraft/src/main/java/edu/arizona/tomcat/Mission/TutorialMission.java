package edu.arizona.tomcat.Mission;

import java.util.UUID;

import com.microsoft.Malmo.Schemas.BlockType;
import com.microsoft.Malmo.Schemas.EntityTypes;
import com.microsoft.Malmo.Schemas.ItemType;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessage;
import edu.arizona.tomcat.Mission.MissionPhase.CompletionStrategy;
import edu.arizona.tomcat.Mission.Goal.CraftItemGoal;
import edu.arizona.tomcat.Mission.Goal.KillEntityGoal;
import edu.arizona.tomcat.Mission.Goal.MissionGoal;
import edu.arizona.tomcat.Mission.Goal.OpenInventoryGoal;
import edu.arizona.tomcat.Mission.Goal.ReachPositionGoal;
import edu.arizona.tomcat.Utils.InventoryHandler;
import edu.arizona.tomcat.World.Drawing;
import edu.arizona.tomcat.World.Room;
import edu.arizona.tomcat.World.TomcatEntity;
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
	private UUID zombieId;

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
		this.killAZombie();		
		this.approachRoom3Phase = new MissionPhase();
		this.approachRoom3Phase.addGoal(new ReachPositionGoal(-11, 2, 2, 1));
		this.addPhase(this.approachRoom3Phase);

	}

	private void openInventory() {
		MissionPhase openInventoryPhase = new MissionPhase(CompletionStrategy.ANY_GOAL, 0, true, "Well Done!", 2, 2);
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
		MissionPhase goToTheMainEntrancePhase = new MissionPhase(CompletionStrategy.ANY_GOAL, 0, true, "Well Done!", 2, 2);
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
		this.zombieId = UUID.randomUUID();
		MissionPhase fightMonster = new MissionPhase(CompletionStrategy.ANY_GOAL, 0, true, "Well Done!", 2, 2);
		MissionGoal goal = new KillEntityGoal(this.zombieId);		
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
				Drawing drawing = new Drawing();
				//				//Plane plane = new Plane(0, 2, 10, 4, 2, 3, BlockType.STONE);
				TomcatEntity zombie = new TomcatEntity(this.zombieId, 0, 2, 10, EntityTypes.ZOMBIE);
				//				Room room = new Room(0, 2, 10, 4, 2, 3, BlockType.STONE, true);
				//				Item door = new Item(10, 2, 0, ItemType.WOODEN_DOOR);
				//				drawing.addObject(room);
				drawing.addObject(zombie);
				//				drawing.addObject(door);
				this.drawingHandler.draw(world, drawing);
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
	protected void beforePhaseTrasition() {
		if (this.currentPhase.equals(this.approachRoom3Phase)) {
			this.blockRoom3 = true;
		}
	}

	@Override
	protected void afterLastPhaseCompletion() {
		// TODO Auto-generated method stub

	}

	@Override
	public void goalAchieved(MissionGoal goal) {
		// TODO Auto-generated method stub

	}

	@Override
	protected void onTimeOut() {
		// TODO Auto-generated method stub

	}

	@Override
	public void handleMessageFromClient(TomcatMessage message) {
		// TODO Auto-generated method stub

	}

	@Override
	public void initMalmoModClientAndServerMission() {
		// TODO Auto-generated method stub

	}

}
