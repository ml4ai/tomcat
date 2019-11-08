package edu.arizona.tomcat.Mission;

import java.util.UUID;

import com.microsoft.Malmo.MalmoMod;
import com.microsoft.Malmo.Schemas.EntityTypes;
import com.microsoft.Malmo.Schemas.ItemType;

import edu.arizona.tomcat.Mission.MissionPhase.CompletionStrategy;
import edu.arizona.tomcat.Mission.Goal.ApproachEntityGoal;
import edu.arizona.tomcat.Mission.Goal.MissionGoal;
import edu.arizona.tomcat.Mission.gui.SARCompletionScreen;
import edu.arizona.tomcat.Utils.InventoryHandler;
import edu.arizona.tomcat.Utils.MinecraftServerHelper;
import edu.arizona.tomcat.World.Drawing;
import edu.arizona.tomcat.World.TomcatEntity;
import io.netty.buffer.ByteBuf;
import net.minecraft.client.Minecraft;
import net.minecraft.util.IThreadListener;
import net.minecraft.world.World;
import net.minecraftforge.fml.common.network.simpleimpl.IMessage;
import net.minecraftforge.fml.common.network.simpleimpl.IMessageHandler;
import net.minecraftforge.fml.common.network.simpleimpl.MessageContext;
import net.minecraftforge.fml.relauncher.Side;

public class SARMission extends Mission {

	private static final int NUMBER_OF_VILLAGERS = 4;
	private static final int MAX_DISTANCE_TO_SAVE_VILLAGER = 1;

	private boolean dynamicInitializationComplete;
	private UUID[] villagersIDs; 
	private int totalVillagersSaved;

	public SARMission() {
		super();
		this.dynamicInitializationComplete = false;
		this.totalVillagersSaved = 0;

		MalmoMod.network.registerMessage(SARMissionCompletionScreenMessageHandler.class, SARMissionCompletionScreenMessage.class, 0, Side.CLIENT);
	}

	@Override
	public void init(World world) {
		// No initialization required
	}

	@Override
	protected void beforePhaseTrasition() {
		// No action to be taken	
	}

	@Override
	protected void afterLastPhaseCompletion() {
		MalmoMod.network.sendTo(new SARMissionCompletionScreenMessage(NUMBER_OF_VILLAGERS), MinecraftServerHelper.getFirstPlayer());
	}

	@Override
	protected void onTimeOut() {
		MalmoMod.network.sendTo(new SARMissionCompletionScreenMessage(this.totalVillagersSaved), MinecraftServerHelper.getFirstPlayer());
	}

	@Override
	protected void createPhases() {
		this.createVillagersIDs();
		MissionPhase rescueVillagersPhase = new MissionPhase(CompletionStrategy.ALL_GOALS, 0, true);	
		rescueVillagersPhase.addInstructionsLine("Save 4 villagers trapped in some of the rooms in the building ahead.");
		rescueVillagersPhase.addInstructionsLine("To save villagers, just get as close as you can from them.");
		rescueVillagersPhase.addInstructionsLine("Take care! You may have to battle some scary creatures on the way.");
		rescueVillagersPhase.addInstructionsLine("You have 10 minutes to accomplish this goal.");
		rescueVillagersPhase.addInstructionsLine("Press OK when you are ready to start.");
		rescueVillagersPhase.setMessageOnCompletion("Mission Accomplished!");
		for(int i = 0; i < NUMBER_OF_VILLAGERS; i++) {
			ApproachEntityGoal goal = new ApproachEntityGoal(this.villagersIDs[i], MAX_DISTANCE_TO_SAVE_VILLAGER);
			rescueVillagersPhase.addGoal(goal);
		}
		//rescueVillagersPhase.addGoal(new ReachPositionGoal(22, 64, 80, 2));
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

			TomcatEntity zombie1 = new TomcatEntity(46, 64, 47, EntityTypes.ZOMBIE);
			TomcatEntity zombie2 = new TomcatEntity(93, 64, 53, EntityTypes.ZOMBIE);
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
			TomcatEntity villager2 = new TomcatEntity(this.villagersIDs[1], 94, 64, 90, EntityTypes.VILLAGER);
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
	 * Add itens to the player's inventory to help him accomplish the mission goals
	 * @param world
	 */
	private void addItensToInventory(World world) {
		InventoryHandler.addItemToInventory(ItemType.IRON_SWORD, 1);
	}

	public static class SARMissionCompletionScreenMessage implements IMessage {

		private int totalVillagersSaved;

		public SARMissionCompletionScreenMessage() {

		}

		public SARMissionCompletionScreenMessage(int totalVillagersSaved) {
			this.totalVillagersSaved = totalVillagersSaved;
		}

		@Override
		public void fromBytes(ByteBuf buf) {
			this.totalVillagersSaved = buf.readInt();
		}

		@Override
		public void toBytes(ByteBuf buf) {
			buf.writeInt(this.totalVillagersSaved);
		}		
	}

	public static class SARMissionCompletionScreenMessageHandler implements IMessageHandler<SARMissionCompletionScreenMessage, IMessage> {

		@Override
		public IMessage onMessage(final SARMissionCompletionScreenMessage message, final MessageContext ctx) {
			IThreadListener mainThread = null;			
			if (ctx.side == Side.CLIENT) {
				mainThread = Minecraft.getMinecraft();

				mainThread.addScheduledTask(new Runnable() {
					@Override
					public void run() {
						SARCompletionScreen teste = new SARCompletionScreen(message.totalVillagersSaved);
						Minecraft.getMinecraft().displayGuiScreen(teste);
					}
				});    
			}
			return null;
		}
	}

	@Override
	public void goalAchieved(MissionGoal goal) {
		this.totalVillagersSaved += 1;
	}
}
