package edu.arizona.tomcat.Mission;

import java.math.BigDecimal;
import java.util.*;

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
import edu.arizona.tomcat.Mission.gui.SelfReportContent;
import edu.arizona.tomcat.Utils.Converter;
import edu.arizona.tomcat.Utils.MinecraftServerHelper;
import edu.arizona.tomcat.World.Drawing;
import edu.arizona.tomcat.World.TomcatEntity;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public class SARMission extends Mission {

	public static final int NUMBER_OF_VILLAGERS = 4;
	private static final int MAX_DISTANCE_TO_SAVE_VILLAGER = 1;	
	private static final BlockPos[] CENTERS_OF_SINGLE_ROOMS = {new BlockPos(46, 64, 47), new BlockPos(93, 64, 53)};
	private static final BlockPos[] CENTERS_OF_INTERNAL_ROOMS = {new BlockPos(52, 67, 89), new BlockPos(95, 64, 90),
			new BlockPos(69, 64, 81), new BlockPos(63, 64, 63)};
	private static final BlockPos[] CENTERS_OF_EXTERNAL_ROOMS = {new BlockPos(57, 64, 61), new BlockPos(72, 64, 75), 
			new BlockPos(88, 64, 87)};
	
	private boolean dynamicInitializationComplete;
	private UUID[] villagersIDs; 
	private int numberOfVillagersSaved;
	
	public SARMission() {
		super();
		this.dynamicInitializationComplete = false;		
		this.numberOfVillagersSaved = 0;
	}
	
	@Override
	protected int getID() {
		return MissionFactory.SEARCH_AND_RESCUE;
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
		this.removeAllEntities();
		RichContent content = RichContent.createFromJson("sar_completion.json");
		content.setTextPlaceholder(0, Integer.toString(this.numberOfVillagersSaved));
		MalmoMod.network.sendTo(new TomcatMessaging.TomcatMessage(TomcatMessageType.SHOW_COMPLETION_SCREEN, new TomcatMessageData(content)), MinecraftServerHelper.getFirstPlayer());
		this.notifyAllAboutMissionEnding("0");
		this.cleanup();
	}

	@Override
	protected void onTimeOut() {
		this.removeAllEntities();
		RichContent content = RichContent.createFromJson("sar_completion.json");
		content.setTextPlaceholder(0, Integer.toString(this.numberOfVillagersSaved));
		MalmoMod.network.sendTo(new TomcatMessaging.TomcatMessage(TomcatMessageType.SHOW_COMPLETION_SCREEN, new TomcatMessageData(content)), MinecraftServerHelper.getFirstPlayer());
		this.notifyAllAboutMissionEnding("1");
		this.cleanup();
	}
	
	public void removeAllEntities() {
		World world = MinecraftServerHelper.getServer().getEntityWorld();
		for(Entity entity : world.getLoadedEntityList()) {
			world.removeEntity(entity);
		}
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
		this.doDynamicInitialization(world);			
	}

	/**
	 * Perform dynamic initializations in the mission
	 * @param world - Minecraft world
	 */
	private void doDynamicInitialization(World world) {
		if(!this.dynamicInitializationComplete) {
			this.spawnEntities(world);
			this.addItensToInventory(world);
			this.dynamicInitializationComplete = true;
		}
	}


	private void spawnEntities(World world){
		int[] villagerPointCoordinates = {52,67,89,95,64,90,93,64,63,94,64,90,46,64,47,52,66,89};
		List<int[]> unusableCoordinateList = new ArrayList<int[]>();
		List<int[]> usableCoordinateList = createCoordinateList(villagerPointCoordinates, unusableCoordinateList);

		this.spawnVillagers(world, usableCoordinateList, unusableCoordinateList);

		int[] enemyPointCoordinates = {46,64,47,93,64,53,57,64,61,72,64,75,88,64,87};
		usableCoordinateList = createCoordinateList(enemyPointCoordinates,unusableCoordinateList);

		this.spawnEnemies(world, usableCoordinateList, unusableCoordinateList);
	}

	/**
	 * Spawn villagers in the mission
	 * @param world - Minecraft world
	 */
	private void spawnVillagers(World world, List<int[]> usableCoordinateList, List<int[]> unusableCoordinateList) {
		try {
			Drawing drawing = new Drawing();

			TomcatEntity villager1 = new TomcatEntity(this.villagersIDs[2], 69, 64, 81, EntityTypes.VILLAGER);
			TomcatEntity villager2 = new TomcatEntity(this.villagersIDs[3], 63, 64, 63, EntityTypes.VILLAGER);
			drawing.addObject(villager1);
			drawing.addObject(villager2);

			TomcatEntity[] villagerSet = new TomcatEntity[2];

			for(TomcatEntity villager : villagerSet){
				int[] currentCoordinates = getRandomCoordinates(usableCoordinateList);
				int x= currentCoordinates[0],y = currentCoordinates[1], z = currentCoordinates[2];

				villager = new TomcatEntity(this.villagersIDs[2], x, y, z, EntityTypes.VILLAGER);

				drawing.addObject(villager);
				usableCoordinateList.remove(currentCoordinates);
				unusableCoordinateList.add(currentCoordinates);
			}
			this.drawingHandler.draw(world, drawing);

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Spawn enemies in the mission
	 * @param world - Minecraft world
	 */
	private void spawnEnemies(World world, List<int[]> usableCoordinateList, List<int[]> unusableCoordinateList) {
		try {
			Drawing drawing = new Drawing();

			TomcatEntity[] enemies = new TomcatEntity[usableCoordinateList.size()];
			for(TomcatEntity enemy : enemies){
				int[] currentCoordinates = getRandomCoordinates(usableCoordinateList);
				int x= currentCoordinates[0],y = currentCoordinates[1], z = currentCoordinates[2];

				double mobRandomizer = Math.random();

				if(mobRandomizer > 0.7){
					enemy = new TomcatEntity(x, y, z, EntityTypes.SKELETON);
				}
				else{
					enemy = new TomcatEntity(	x, y, z, EntityTypes.ZOMBIE);
				}

				drawing.addObject(enemy);
				usableCoordinateList.remove(currentCoordinates);
				unusableCoordinateList.add(currentCoordinates);
			}
			this.drawingHandler.draw(world, drawing);

		} catch (Exception e) {
			e.printStackTrace();
		}
	}


	private static int[] getRandomCoordinates(List<int[]> coordinateList){
		int randomIndex = (int)(Math.random())*coordinateList.size();
		return coordinateList.remove(randomIndex);

	}

	private static List<int[]> createCoordinateList (int[] threePointCoordinates, List<int[]> unusableCoordinateList){
	    List<int[]> coordinateList = new ArrayList<int[]>();
	    int[] currentCoordinates = new int[3];
	    int j=0;

	    for(int i=0;i<threePointCoordinates.length;i++){
	        if(i%3==0){
	        	if(!(unusableCoordinateList.contains(currentCoordinates))){
					coordinateList.add(currentCoordinates);
				}
				currentCoordinates = new int[3];
	            j=0;
            }
	        else{
				currentCoordinates[j++]=threePointCoordinates[i];
            }
        }
	    return coordinateList;
    }

	/**
	 * Add items to the player's inventory to help them accomplish the mission goals
	 * @param world
	 */
	private void addItensToInventory(World world) {
		//InventoryHandler.addItemToInventory(ItemType.IRON_SWORD, 1);
	}

	@Override
	public void goalAchieved(World world, MissionGoal goal) {
		if (goal instanceof ApproachEntityGoal) {
			this.handleVillagerRescue(world, (ApproachEntityGoal) goal);
		}
	}
	
	private void handleVillagerRescue(World world, ApproachEntityGoal goal) {
		this.numberOfVillagersSaved++;
		this.addToDeletion(goal.getEntity(), world.getTotalWorldTime());
		MalmoMod.network.sendTo(new TomcatMessaging.TomcatMessage(TomcatMessageType.VILLAGER_SAVED), MinecraftServerHelper.getFirstPlayer());
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

	@Override
	protected boolean hasSelfReport() {
		return true;
	}

	@Override
	protected SelfReportContent getSelfReportContent(World world) {
		String id = Long.toString(world.getTotalWorldTime());
		return SelfReportContent.createFromJson(id, "self_report1.json");
	}

	@Override
	protected void onPlayerDeath() {
		this.onTimeOut();
	}
}
