package edu.arizona.tomcat.Mission;

import java.math.BigDecimal;
import java.util.UUID;

import com.microsoft.Malmo.MalmoMod;
import com.microsoft.Malmo.Schemas.BlockType;
import com.microsoft.Malmo.Schemas.EntityTypes;
import com.microsoft.Malmo.Schemas.ItemType;
import com.microsoft.Malmo.Schemas.PosAndDirection;

import edu.arizona.tomcat.Messaging.TomcatMessageData;
import edu.arizona.tomcat.Messaging.TomcatMessaging;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessageType;
import edu.arizona.tomcat.Mission.MissionPhase.CompletionStrategy;
import edu.arizona.tomcat.Mission.Client.ClientMission;
import edu.arizona.tomcat.Mission.Client.TutorialClientMission;
import edu.arizona.tomcat.Mission.Goal.ApproachEntityGoal;
import edu.arizona.tomcat.Mission.Goal.CraftItemGoal;
import edu.arizona.tomcat.Mission.Goal.KillEntityGoal;
import edu.arizona.tomcat.Mission.Goal.MissionGoal;
import edu.arizona.tomcat.Mission.Goal.ReachPositionGoal;
import edu.arizona.tomcat.Mission.gui.RichContent;
import edu.arizona.tomcat.Mission.gui.SelfReportContent;
import edu.arizona.tomcat.Utils.InventoryHandler;
import edu.arizona.tomcat.Utils.MinecraftServerHelper;
import edu.arizona.tomcat.World.Drawing;
import edu.arizona.tomcat.World.TomcatEntity;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.world.World;

public class TutorialMission extends Mission {

	private static final int MAX_DISTANCE_TO_SAVE_VILLAGER = 1;
	private static final float SECONDS_PER_CAMERA_VIEW = 1.5f;  

	private double viewTime;
	private boolean shouldSpawnSkeletonInTheArena;
	private boolean shouldSpawnZombieInsideTheBuilding;
	private boolean shouldSpawnVillagerInsideTheBuilding;
	private boolean shouldaddMaterialsToUsersInventory;
	private MissionPhase approachPoolsPhase;
	private MissionPhase enterTheArenaPhase;
	private MissionPhase killSkeletonPhase;
	private MissionPhase killZombiePhase;
	private UUID skeletonUUID;
	private UUID zombieUUID;
	private UUID villagerUUID;

	public static final int NUMBER_OF_VILLAGERS = 1;

	public TutorialMission() {
		super();
		this.viewTime = 0;
		this.shouldSpawnSkeletonInTheArena = false;
		this.shouldSpawnZombieInsideTheBuilding = false;
		this.shouldSpawnVillagerInsideTheBuilding = false;
		this.skeletonUUID = UUID.randomUUID();
		this.zombieUUID = UUID.randomUUID();
		this.villagerUUID = UUID.randomUUID();
	}

	@Override
	protected int getID() {
		return MissionFactory.TUTORIAL;
	}

	@Override
	public void init(World world) {
		super.init(world);
	}

	@Override
	protected void createPhases() {
		this.addApproachPoolsPhase();
		this.addCraftItemPhase();
		this.addApproachEntitiesPhase();
		this.addEnterTheArenaPhase();
		this.addKillSkeletonPhase();
		this.addKillZombiePhase();
		this.addSaveVillagerPhase();
		this.addLeaveTheBuildingPhase();
	}

	/**
	 * Creates a phase in the mission where the objective is to locate the pools of water and lava in the world
	 */
	private void addApproachPoolsPhase() {
		RichContent instructions = RichContent.createFromJson("tutorial_instructions_pits.json");
		this.approachPoolsPhase = new MissionPhase(instructions, CompletionStrategy.ALL_GOALS, 5, true, "Well Done!", 0, 2);
		this.approachPoolsPhase.addGoal(new ReachPositionGoal(-635, 4, 1582, 2));
		this.addPhase(approachPoolsPhase);
	}

	/**
	 * Creates a phase in the mission where the objective is to craft a wooden item in the world
	 */
	private void addCraftItemPhase() {
		RichContent instructions = RichContent.createFromJson("tutorial_instruction_crafting.json");
		MissionPhase craftItemPhase = new MissionPhase(instructions, CompletionStrategy.ALL_GOALS, 5, true, "Well Done!", 0, 2);
		craftItemPhase.addGoal(new CraftItemGoal(ItemType.WOODEN_AXE));
		this.addPhase(craftItemPhase);
	}

	/**
	 * Creates a phase in the mission where the objective is to locate the entities in the world
	 */
	private void addApproachEntitiesPhase() {
		RichContent instructions = RichContent.createFromJson("tutorial_instructions_entities.json");
		MissionPhase approachEntitiesPhase = new MissionPhase(instructions, CompletionStrategy.ALL_GOALS, 5, true, "Well Done!", 0, 2);
		approachEntitiesPhase.addGoal(new ReachPositionGoal(-615, 4, 1585, 3));
		addPhase(approachEntitiesPhase);
	}

	/**
	 * Creates a phase in the mission where the objective is to go to the center of the arena
	 */
	private void addEnterTheArenaPhase() {
		RichContent instructions = RichContent.createFromJson("tutorial_instructions_arena.json");
		this.enterTheArenaPhase = new MissionPhase(instructions, CompletionStrategy.ALL_GOALS, 10, true, "Well Done!", 0, 2);
		this.enterTheArenaPhase.addGoal(new ReachPositionGoal(-623, 4, 1600, 2));
		this.addPhase(this.enterTheArenaPhase);
	}

	/**
	 * Creates a phase in the mission where the objective is to battle and kill a skeleton
	 */
	private void addKillSkeletonPhase() {
		RichContent instructions = RichContent.createFromJson("tutorial_instructions_skeleton.json");
		this.killSkeletonPhase = new MissionPhase(instructions, CompletionStrategy.ALL_GOALS, 0, true, "Well Done!", 0, 2);
		this.killSkeletonPhase.addGoal(new KillEntityGoal(this.skeletonUUID));
		this.addPhase(this.killSkeletonPhase);
	}

	/**
	 * Creates a phase in the mission where the objective is to battle and kill a zombie
	 */
	private void addKillZombiePhase() {
		RichContent instructions = RichContent.createFromJson("tutorial_instructions_zombie.json");
		this.killZombiePhase = new MissionPhase(instructions, CompletionStrategy.ALL_GOALS, 0, true, "Well Done!", 0, 2);
		this.killZombiePhase.addGoal(new KillEntityGoal(this.zombieUUID));
		this.addPhase(this.killZombiePhase);
	}

	/**
	 * Creates a phase in the mission where the objective is to save a villager
	 */
	private void addSaveVillagerPhase() {
		RichContent instructions = RichContent.createFromJson("tutorial_instructions_villager.json");
		MissionPhase saveVillagerPhase = new MissionPhase(instructions, CompletionStrategy.ALL_GOALS, 0, true, "Well Done!", 0, 2);
		saveVillagerPhase.addGoal(new ApproachEntityGoal(this.villagerUUID, MAX_DISTANCE_TO_SAVE_VILLAGER));
		this.addPhase(saveVillagerPhase);
	}

	/**
	 * Creates a phase in the mission where the objective is leave the building
	 */
	private void addLeaveTheBuildingPhase() {
		RichContent instructions = RichContent.createFromJson("tutorial_instructions_end.json");
		MissionPhase leaveTheBuildingPhase = new MissionPhase(instructions, CompletionStrategy.ALL_GOALS, 0, true, "Well Done!", 0, 2);
		leaveTheBuildingPhase.addGoal(new ReachPositionGoal(-623, 4, 1579, 2));
		addPhase(leaveTheBuildingPhase);
	}


	@Override
	protected void updateScene(World world) {
		this.changePlayerPerspective();
		this.addMaterialsToUsersInventory(world);
		this.spawnSkeletonInTheArena(world);
		this.spawnZombieInsideTheBuilding(world);
		this.spawnVillagerInsideTheBuilding(world);	
	}

	/**
	 *  Cycles through Minecraft player perspectives
	 */
	private void changePlayerPerspective() {
		if (this.viewTime <= 2*SECONDS_PER_CAMERA_VIEW) {
			/* viewTime is in seconds. The player stays in each view mode for 1.5 seconds.
		     20 Minecraft ticks equal 1 real second. viewTime is incremented by 0.05 till 30 such ticks (1.5 second)
		     have passed for each view */

			double roundedTime = Math.round(this.viewTime*100.0)/100.0;

			if (roundedTime == 0.00) {
				MalmoMod.network.sendTo(new TomcatMessaging.TomcatMessage(TomcatMessageType.VIEW_CHANGED), MinecraftServerHelper.getFirstPlayer());
				// third person back view
			}
			else if(roundedTime == SECONDS_PER_CAMERA_VIEW){
				MalmoMod.network.sendTo(new TomcatMessaging.TomcatMessage(TomcatMessageType.VIEW_CHANGED), MinecraftServerHelper.getFirstPlayer());
				// third person front view
			}
			else if (roundedTime == 2*SECONDS_PER_CAMERA_VIEW) {
				MalmoMod.network.sendTo(new TomcatMessaging.TomcatMessage(TomcatMessageType.VIEW_CHANGED), MinecraftServerHelper.getFirstPlayer());
				// first person view
			}
			this.viewTime += 0.05;
		}
	}

	/**
	 * Spawn skeleton in the arena
	 * @param world - Minecraft world
	 */
	private void spawnSkeletonInTheArena(World world) {	
		if (this.shouldSpawnSkeletonInTheArena) {
			try {
				Drawing drawing = new Drawing();
				TomcatEntity skeleton = new TomcatEntity(this.skeletonUUID, -620, 4, 1596, EntityTypes.SKELETON);
				drawing.addObject(skeleton);
				this.drawingHandler.draw(world, drawing);			
			} catch (Exception e) {
				e.printStackTrace();
			}
			this.shouldSpawnSkeletonInTheArena = false;
		}
	}	

	/**
	 * Spawn zombie inside the building
	 * @param world - Minecraft world
	 */
	private void spawnZombieInsideTheBuilding(World world) {	
		if (this.shouldSpawnZombieInsideTheBuilding) {
			try {
				Drawing drawing = new Drawing();

				TomcatEntity zombie = new TomcatEntity(this.zombieUUID, -623, 4, 1571, EntityTypes.ZOMBIE);			

				drawing.addObject(zombie);
				this.drawingHandler.draw(world, drawing);			
			} catch (Exception e) {
				e.printStackTrace();
			}
			this.shouldSpawnZombieInsideTheBuilding = false;
		}
	}	

	/**
	 * Spawn villager inside the building
	 * @param world - Minecraft world
	 */
	private void spawnVillagerInsideTheBuilding(World world) {	
		if (this.shouldSpawnVillagerInsideTheBuilding) {
			try {
				Drawing drawing = new Drawing();

				TomcatEntity villager = new TomcatEntity(this.villagerUUID, -631, 4, 1570, EntityTypes.VILLAGER);			

				drawing.addObject(villager);
				this.drawingHandler.draw(world, drawing);			
			} catch (Exception e) {
				e.printStackTrace();
			}
			this.shouldSpawnVillagerInsideTheBuilding = false;
		}
	}

	/**
	 * Add three planks and two sticks inside the player's inventory.
	 * @param world - Minecraft world
	 */
	private void addMaterialsToUsersInventory(World world) {
		if(this.shouldaddMaterialsToUsersInventory){
			InventoryHandler.addBlockToInventory(BlockType.PLANKS, 3);
			InventoryHandler.addItemToInventory(ItemType.STICK, 2);
			this.shouldaddMaterialsToUsersInventory = false;
		}
	}

	@Override
	protected void afterLastPhaseCompletion() {
		RichContent content = RichContent.createFromJson("tutorial_completion.json");
		MalmoMod.network.sendTo(new TomcatMessaging.TomcatMessage(TomcatMessageType.SHOW_COMPLETION_SCREEN, new TomcatMessageData(content)), MinecraftServerHelper.getFirstPlayer());
		this.notifyAllAboutMissionEnding("0");
	}

	@Override
	public void goalAchieved(World world, MissionGoal goal) {
		if (goal instanceof ApproachEntityGoal) {
			this.handleVillagerRescue(world, (ApproachEntityGoal) goal);			
		}
	}
	
	private void handleVillagerRescue(World world, ApproachEntityGoal goal) {
		this.addToDeletion(goal.getEntity(), world.getTotalWorldTime());
		MalmoMod.network.sendTo(new TomcatMessaging.TomcatMessage(TomcatMessageType.VILLAGER_SAVED), MinecraftServerHelper.getFirstPlayer());
	}

	@Override
	protected void onTimeOut() {
		// There's no timeout in the tutorial mission
	}	

	@Override
	public ClientMission getClientMissionInstance() {
		return new TutorialClientMission();
	}

	@Override
	protected void beforePhaseTrasition() {
		if (this.currentPhase.equals(this.enterTheArenaPhase)) {
			this.shouldSpawnSkeletonInTheArena = true;
		} else if (this.currentPhase.equals(this.killSkeletonPhase)) {
			this.shouldSpawnZombieInsideTheBuilding = true;
		} else if (this.currentPhase.equals(this.killZombiePhase)){
			this.shouldSpawnVillagerInsideTheBuilding = true;
		} else if(this.currentPhase.equals(this.approachPoolsPhase)){
			this.shouldaddMaterialsToUsersInventory = true;
		}
	}

	@Override
	public void setTimeLimitInSeconds(long timeLimitInSeconds) {
		// Tutorial mission has no time limit
		this.timeLimitInSeconds = -1;
	}

	@Override
	public PosAndDirection getPlayersInitialPositionAndDirection(EntityPlayerMP player) {
		PosAndDirection positionAndDirection = new PosAndDirection();
		positionAndDirection.setX(new BigDecimal(-623));
		positionAndDirection.setY(new BigDecimal(4));
		positionAndDirection.setZ(new BigDecimal(1584));
		return positionAndDirection;
	}

	@Override
	protected boolean hasSelfReport() {
		return false;
	}

	@Override
	protected SelfReportContent getSelfReportContent(World world) {
		return null;
	}

	@Override
	protected void onPlayerDeath() {
		RichContent content = RichContent.createFromJson("tutorial_completion_fail.json");
		MalmoMod.network.sendTo(new TomcatMessaging.TomcatMessage(TomcatMessageType.SHOW_COMPLETION_SCREEN, new TomcatMessageData(content)), MinecraftServerHelper.getFirstPlayer());
		this.notifyAllAboutMissionEnding("1");
		this.cleanup();
	}

}
