package edu.arizona.tomcat.Mission;

import java.math.BigDecimal;
import java.util.UUID;

import com.microsoft.Malmo.MalmoMod;
import com.microsoft.Malmo.Schemas.EntityTypes;
import com.microsoft.Malmo.Schemas.PosAndDirection;

import edu.arizona.tomcat.Messaging.TomcatMessaging;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessageType;
import edu.arizona.tomcat.Mission.MissionPhase.CompletionStrategy;
import edu.arizona.tomcat.Mission.Client.ClientMission;
import edu.arizona.tomcat.Mission.Client.TutorialClientMission;
import edu.arizona.tomcat.Mission.Goal.ApproachEntityGoal;
import edu.arizona.tomcat.Mission.Goal.KillEntityGoal;
import edu.arizona.tomcat.Mission.Goal.MissionGoal;
import edu.arizona.tomcat.Mission.Goal.ReachPositionGoal;
import edu.arizona.tomcat.Utils.MinecraftServerHelper;
import edu.arizona.tomcat.World.Drawing;
import edu.arizona.tomcat.World.TomcatEntity;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.world.World;

public class TutorialMission extends Mission {
	
	public static final int NUMBER_OF_VILLAGERS = 1;
	private static final int MAX_DISTANCE_TO_SAVE_VILLAGER = 1;

	private boolean shouldSpawnSkeletonInTheArena;
	private boolean shouldSpawnZombieInsideTheBuilding;
	private boolean shouldSpawnVillagerInsideTheBuilding;
	private MissionPhase enterTheArenaPhase;
	private MissionPhase killSkeletonPhase;
	private MissionPhase killZombiePhase;
	private UUID skeletonUUID;
	private UUID zombieUUID;
	private UUID villagerUUID;
	
	public TutorialMission() {
		super();
		this.shouldSpawnSkeletonInTheArena = false;
		this.shouldSpawnZombieInsideTheBuilding = false;
		this.shouldSpawnVillagerInsideTheBuilding = false;
		this.skeletonUUID = UUID.randomUUID();
		this.zombieUUID = UUID.randomUUID();
		this.villagerUUID = UUID.randomUUID();
	}
	
	@Override
	public void init(World world) {
		super.init(world);		
	}

	@Override
	protected void createPhases() {		
		this.addApproachPoolsPhase();
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
		MissionPhase approachPoolsPhase = new MissionPhase(CompletionStrategy.ALL_GOALS, 0, true, "You got it!", 0, 2);	
		approachPoolsPhase.addInstructionsLine("Approach the lava and water tanks.");
		approachPoolsPhase.addInstructionsLine("Use the keys A,W,D and S to move around.");
		approachPoolsPhase.addInstructionsLine("Take care! If you fall into the lava tank, you die.");
		approachPoolsPhase.addInstructionsLine("If you fall into the water tank, press space until you reach the surface.");
		approachPoolsPhase.addInstructionsLine("");
		approachPoolsPhase.addInstructionsLine("Click OK when you are ready.");
		approachPoolsPhase.addGoal(new ReachPositionGoal(-635, 4, 1582, 2));
		this.addPhase(approachPoolsPhase);
	}
	
	/**
	 * Creates a phase in the mission where the objective is to locate the entities in the world
	 */
	private void addApproachEntitiesPhase() {
		MissionPhase approachEntitiesPhase = new MissionPhase(CompletionStrategy.ALL_GOALS, 5, true, "You got it!", 0, 2);	
		approachEntitiesPhase.addInstructionsLine("Take a closer look at a villager, a skeleton and a zombie.");
		approachEntitiesPhase.addInstructionsLine("Pay attention to their appearance.");
		approachEntitiesPhase.addInstructionsLine("You might need to distinguish them well in the future.");
		approachEntitiesPhase.addInstructionsLine("");
		approachEntitiesPhase.addInstructionsLine("Click OK when you are ready.");
		approachEntitiesPhase.addGoal(new ReachPositionGoal(-615, 4, 1585, 3));
		addPhase(approachEntitiesPhase);
	}
	
	/**
	 * Creates a phase in the mission where the objective is to go to the center of the arena
	 */
	private void addEnterTheArenaPhase() {
		this.enterTheArenaPhase = new MissionPhase(CompletionStrategy.ALL_GOALS, 10, true, "You got it!", 0, 2);	
		this.enterTheArenaPhase.addInstructionsLine("Go to the center of the combat arena.");
		this.enterTheArenaPhase.addInstructionsLine("To open the gate, right-click on it.");
		this.enterTheArenaPhase.addInstructionsLine("");
		this.enterTheArenaPhase.addInstructionsLine("Click OK when you are ready.");
		this.enterTheArenaPhase.addGoal(new ReachPositionGoal(-623, 4, 1600, 2));
		this.addPhase(this.enterTheArenaPhase);
	}
	
	/**
	 * Creates a phase in the mission where the objective is to battle and kill a skeleton
	 */
	private void addKillSkeletonPhase() {
		this.killSkeletonPhase = new MissionPhase(CompletionStrategy.ALL_GOALS, 0, true, "You got it!", 0, 2);	
		this.killSkeletonPhase.addInstructionsLine("Battle and kill the skeleton behind you.");		
		this.killSkeletonPhase.addInstructionsLine("Left-click to attack him.");
		this.killSkeletonPhase.addInstructionsLine("You can also try to take him outside the arena. He will burn under the sun.");
		this.killSkeletonPhase.addInstructionsLine("");
		this.killSkeletonPhase.addInstructionsLine("Click OK when you are ready.");
		this.killSkeletonPhase.addGoal(new KillEntityGoal(this.skeletonUUID));
		this.addPhase(this.killSkeletonPhase);
	}
	
	/**
	 * Creates a phase in the mission where the objective is to battle and kill a zombie
	 */
	private void addKillZombiePhase() {
		this.killZombiePhase = new MissionPhase(CompletionStrategy.ALL_GOALS, 0, true, "You got it!", 0, 2);	
		this.killZombiePhase.addInstructionsLine("Leave the arena and kill the zombie inside the building.");		
		this.killZombiePhase.addInstructionsLine("To open an iron door, right-click on the switch close to it.");
		this.killZombiePhase.addInstructionsLine("Zombies also burn under the sun.");
		this.killZombiePhase.addInstructionsLine("");
		this.killZombiePhase.addInstructionsLine("Click OK when you are ready.");
		this.killZombiePhase.addGoal(new KillEntityGoal(this.zombieUUID));
		this.addPhase(this.killZombiePhase);
	}
	
	/**
	 * Creates a phase in the mission where the objective is to save a villager
	 */
	private void addSaveVillagerPhase() {
		MissionPhase saveVillagerPhase = new MissionPhase(CompletionStrategy.ALL_GOALS, 0, true, "You got it!", 0, 2);	
		saveVillagerPhase.addInstructionsLine("Open the wooden door to access the other room and rescue a villager.");		
		saveVillagerPhase.addInstructionsLine("To open a wooden door, just right-click on it.");
		saveVillagerPhase.addInstructionsLine("Rescue the villagger by bumping into him.");
		saveVillagerPhase.addInstructionsLine("");
		saveVillagerPhase.addInstructionsLine("Click OK when you are ready.");
		saveVillagerPhase.addGoal(new ApproachEntityGoal(this.villagerUUID, MAX_DISTANCE_TO_SAVE_VILLAGER));
		this.addPhase(saveVillagerPhase);
	}
	
	/**
	 * Creates a phase in the mission where the objective is leave the building
	 */
	private void addLeaveTheBuildingPhase() {
		MissionPhase leaveTheBuildingPhase = new MissionPhase(CompletionStrategy.ALL_GOALS, 0, true, "You got it!", 0, 2);	
		leaveTheBuildingPhase.addInstructionsLine("Everytime you save a villager, the progress bar in the top-left");
		leaveTheBuildingPhase.addInstructionsLine("corner of your screen increases.");
		leaveTheBuildingPhase.addInstructionsLine("Your bar is full because you saved all the villagers in this mission.");
		leaveTheBuildingPhase.addInstructionsLine("Now, get out of the building.");
		leaveTheBuildingPhase.addInstructionsLine("");
		leaveTheBuildingPhase.addInstructionsLine("Click OK when you are ready.");
		leaveTheBuildingPhase.addGoal(new ReachPositionGoal(-623, 4, 1579, 2));
		addPhase(leaveTheBuildingPhase);
	}

	
	@Override
	protected void updateScene(World world) {
		if (this.shouldSpawnSkeletonInTheArena) {
			this.spawnSkeletonInTheArena(world);
			this.shouldSpawnSkeletonInTheArena = false;
		}	
		
		if (this.shouldSpawnZombieInsideTheBuilding) {
			this.spawnZombieInsideTheBuilding(world);
			this.shouldSpawnZombieInsideTheBuilding = false;
		}	
		
		if (this.shouldSpawnVillagerInsideTheBuilding) {
			this.spawnVillagerInsideTheBuilding(world);
			this.shouldSpawnVillagerInsideTheBuilding = false;
		}		
		
		//System.out.println("===========>Player's position: " + MinecraftServerHelper.getFirstPlayer().getPosition().toString());
		
	}
		
	/**
	 * Spawn skeleton in the arena
	 * @param world - Minecraft world
	 */
	private void spawnSkeletonInTheArena(World world) {		
		try {
			Drawing drawing = new Drawing();

			TomcatEntity skeleton = new TomcatEntity(this.skeletonUUID, -620, 4, 1596, EntityTypes.SKELETON);			

			drawing.addObject(skeleton);
			this.drawingHandler.draw(world, drawing);			
		} catch (Exception e) {
			e.printStackTrace();
		}
	}	
	
	/**
	 * Spawn zombie inside the building
	 * @param world - Minecraft world
	 */
	private void spawnZombieInsideTheBuilding(World world) {		
		try {
			Drawing drawing = new Drawing();

			TomcatEntity zombie = new TomcatEntity(this.zombieUUID, -623, 4, 1571, EntityTypes.ZOMBIE);			

			drawing.addObject(zombie);
			this.drawingHandler.draw(world, drawing);			
		} catch (Exception e) {
			e.printStackTrace();
		}
	}	
	
	/**
	 * Spawn villager inside the building
	 * @param world - Minecraft world
	 */
	private void spawnVillagerInsideTheBuilding(World world) {		
		try {
			Drawing drawing = new Drawing();

			TomcatEntity villager = new TomcatEntity(this.villagerUUID, -631, 4, 1570, EntityTypes.VILLAGER);			

			drawing.addObject(villager);
			this.drawingHandler.draw(world, drawing);			
		} catch (Exception e) {
			e.printStackTrace();
		}
	}	

	@Override
	protected void afterLastPhaseCompletion() {
		MalmoMod.network.sendTo(new TomcatMessaging.TomcatMessage(TomcatMessageType.SHOW_COMPLETION_SCREEN), MinecraftServerHelper.getFirstPlayer());
	}

	@Override
	public void goalAchieved(MissionGoal goal) {
		if (goal instanceof ApproachEntityGoal) {
			MalmoMod.network.sendTo(new TomcatMessaging.TomcatMessage(TomcatMessageType.VILLAGER_SAVED), MinecraftServerHelper.getFirstPlayer());
		}
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

}
