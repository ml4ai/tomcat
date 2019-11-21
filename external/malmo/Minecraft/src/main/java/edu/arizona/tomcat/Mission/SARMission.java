package edu.arizona.tomcat.Mission;

import java.math.BigDecimal;
import java.util.UUID;

import com.microsoft.Malmo.MalmoMod;
import com.microsoft.Malmo.Schemas.EntityTypes;
import com.microsoft.Malmo.Schemas.PosAndDirection;

import edu.arizona.tomcat.Messaging.TomcatMessageData;
import edu.arizona.tomcat.Messaging.TomcatMessaging;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessageType;
import edu.arizona.tomcat.Mission.MissionPhase.CompletionStrategy;
import edu.arizona.tomcat.Mission.Client.ClientMission;
import edu.arizona.tomcat.Mission.Client.SARClientMission;
import edu.arizona.tomcat.Mission.Goal.ApproachEntityGoal;
import edu.arizona.tomcat.Mission.Goal.MissionGoal;
import edu.arizona.tomcat.Mission.gui.RichContent;
import edu.arizona.tomcat.Utils.Converter;
import edu.arizona.tomcat.Utils.MinecraftServerHelper;
import edu.arizona.tomcat.World.Drawing;
import edu.arizona.tomcat.World.TomcatEntity;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.world.World;

public class SARMission extends Mission {

	public static final int NUMBER_OF_VILLAGERS = 4;
	private static final int MAX_DISTANCE_TO_SAVE_VILLAGER = 1;

	private boolean dynamicInitializationComplete;
	private UUID[] villagersIDs; 
	private int numberOfVillagersSaved;

	public SARMission() {
		super();
		this.dynamicInitializationComplete = false;		
		this.numberOfVillagersSaved = 0;
	}
	
	@Override
	public void init(World world) {		
		super.init(world);	
	}

	@Override
	protected void beforePhaseTrasition() {
		// No action to be taken	
	}

	@Override
	protected void afterLastPhaseCompletion() {
		RichContent content = RichContent.createFromJson("sar_completion.json");
		content.setTextPlaceholder(0, Integer.toString(this.numberOfVillagersSaved));
		MalmoMod.network.sendTo(new TomcatMessaging.TomcatMessage(TomcatMessageType.SHOW_COMPLETION_SCREEN, new TomcatMessageData(content)), MinecraftServerHelper.getFirstPlayer());
	}

	@Override
	protected void onTimeOut() {
		RichContent content = RichContent.createFromJson("sar_completion.json");
		content.setTextPlaceholder(0, Integer.toString(this.numberOfVillagersSaved));
		MalmoMod.network.sendTo(new TomcatMessaging.TomcatMessage(TomcatMessageType.SHOW_COMPLETION_SCREEN, new TomcatMessageData(content)), MinecraftServerHelper.getFirstPlayer());
	}

	@Override
	protected void createPhases() {
		this.createVillagersIDs();
		RichContent instructions = RichContent.createFromJson("sar_instructions.json");
		instructions.setTextPlaceholder(0, Integer.toString(NUMBER_OF_VILLAGERS));
		instructions.setTextPlaceholder(1, Converter.secondsToString(this.timeLimitInSeconds, true));
		MissionPhase rescueVillagersPhase = new MissionPhase(instructions, CompletionStrategy.ALL_GOALS, 0);	
		for(int i = 0; i < NUMBER_OF_VILLAGERS; i++) {
			ApproachEntityGoal goal = new ApproachEntityGoal(this.villagersIDs[i], MAX_DISTANCE_TO_SAVE_VILLAGER);
			rescueVillagersPhase.addGoal(goal);
		}
		this.addPhase(rescueVillagersPhase);
	}

	/**
	 * Create unique IDs for each one of the villagers
	 */

	private void createVillagersIDs() {
		this.villagersIDs = new UUID[NUMBER_OF_VILLAGERS];
		for(int i = 0; i < NUMBER_OF_VILLAGERS; i++) {
			this.villagersIDs[i] = UUID.randomUUID();
		}
	}

	@Override
	protected void updateScene(World world) {
		if(!this.dynamicInitializationComplete) {
			this.doDynamicInitialization(world);
		}		
	}

	/**
	 * Perform dynamic initializations in the mission
	 * @param world - Minecraft world
	 */
	private void doDynamicInitialization(World world) {		
		this.spawnEntities(world);
		this.addItensToInventory(world);
		this.dynamicInitializationComplete = true;
	}

	/**
	 * Spawn entities in the mission when it starts
	 */
	private void spawnEntities(World world) {
		this.spawnEnemies(world);
		this.spawnVillagers(world);
	}

	/**
	 * Spawn enemies in the mission
	 * @param world - Minecraft world
	 */
	private void spawnEnemies(World world) {		
		try {
			Drawing drawing = new Drawing();

			TomcatEntity zombie1 = new TomcatEntity(46, 64, 47, EntityTypes.SKELETON);
			TomcatEntity zombie2 = new TomcatEntity(93, 64, 53, EntityTypes.SKELETON);
			TomcatEntity zombie3 = new TomcatEntity(57, 64, 61, EntityTypes.ZOMBIE);
			TomcatEntity zombie4 = new TomcatEntity(72, 64, 75, EntityTypes.ZOMBIE);
			TomcatEntity zombie5 = new TomcatEntity(88, 64, 87, EntityTypes.ZOMBIE);			

			drawing.addObject(zombie1);
			drawing.addObject(zombie2);
			drawing.addObject(zombie3);
			drawing.addObject(zombie4);
			drawing.addObject(zombie5);
			this.drawingHandler.draw(world, drawing);			
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Spawn villagers in the mission
	 * @param world - Minecraft world
	 */
	private void spawnVillagers(World world) {
		try {
			Drawing drawing = new Drawing();

			TomcatEntity villager1 = new TomcatEntity(this.villagersIDs[0], 52, 67, 89, EntityTypes.VILLAGER);
			TomcatEntity villager2 = new TomcatEntity(this.villagersIDs[1], 95, 64, 90, EntityTypes.VILLAGER);
			TomcatEntity villager3 = new TomcatEntity(this.villagersIDs[2], 69, 64, 81, EntityTypes.VILLAGER);
			TomcatEntity villager4 = new TomcatEntity(this.villagersIDs[3], 63, 64, 63, EntityTypes.VILLAGER);		

			drawing.addObject(villager1);
			drawing.addObject(villager2);
			drawing.addObject(villager3);
			drawing.addObject(villager4);
			this.drawingHandler.draw(world, drawing);			
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Add items to the player's inventory to help them accomplish the mission goals
	 * @param world
	 */
	private void addItensToInventory(World world) {
		//InventoryHandler.addItemToInventory(ItemType.IRON_SWORD, 1);
	}

	@Override
	public void goalAchieved(MissionGoal goal) {
		if (goal instanceof ApproachEntityGoal) {
			this.numberOfVillagersSaved++;
			MalmoMod.network.sendTo(new TomcatMessaging.TomcatMessage(TomcatMessageType.VILLAGER_SAVED), MinecraftServerHelper.getFirstPlayer());
		}
	}

	@Override
	public ClientMission getClientMissionInstance() {
		return new SARClientMission();		
	}
	
	@Override
	public PosAndDirection getPlayersInitialPositionAndDirection(EntityPlayerMP player) {
		PosAndDirection positionAndDirection = new PosAndDirection();
		positionAndDirection.setX(new BigDecimal(22));
		positionAndDirection.setY(new BigDecimal(64));
		positionAndDirection.setZ(new BigDecimal(73));
		positionAndDirection.setYaw(new BigDecimal(-90));
		return positionAndDirection;
	}
}
