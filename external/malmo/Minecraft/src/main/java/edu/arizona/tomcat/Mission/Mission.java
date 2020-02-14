package edu.arizona.tomcat.Mission;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Set;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.microsoft.Malmo.MalmoMod;
import com.microsoft.Malmo.Schemas.PosAndDirection;

import edu.arizona.tomcat.Emotion.EmotionHandler;
import edu.arizona.tomcat.Messaging.TomcatMessageData;
import edu.arizona.tomcat.Messaging.TomcatMessaging;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessage;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessageType;
import edu.arizona.tomcat.Mission.Client.ClientMission;
import edu.arizona.tomcat.Mission.gui.FeedbackListener;
import edu.arizona.tomcat.Mission.gui.SelfReportContent;
import edu.arizona.tomcat.Utils.Converter;
import edu.arizona.tomcat.World.DrawingHandler;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.world.World;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.CommandEvent;
import net.minecraftforge.event.entity.living.LivingDeathEvent;
import net.minecraftforge.fml.common.eventhandler.EventPriority;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;

public abstract class Mission implements FeedbackListener, PhaseListener {
	
	private static final long ENTITY_DELETION_DELAY = 1;
	private static final String SELF_REPORT_FOLDER = "saves/self_reports";
	private HashMap<String, MissionSelfReport> selfReportPerPlayer;

	protected static final int REMAINING_SECONDS_ALERT = 30;
	private boolean canShowSelfReport;
	protected int numberOfPhasesCompleted; 
	protected long timeLimitInSeconds;
	protected long selfReportPromptTimeInSeconds;
	protected long initialWorldTime;	
	protected DrawingHandler drawingHandler;
	protected EmotionHandler.Emotion currentEmotion;
	protected MissionPhase currentPhase;
	protected ArrayList<MissionPhase> phases;
	protected ArrayList<MissionListener> listeners;	
	protected HashMap<Entity, Long> entitiesToRemove;

	/**
	 * Abstract constructor for initialization of the drawing handler
	 */
	protected Mission() {
		this.drawingHandler = DrawingHandler.getInstance();
		this.listeners = new ArrayList<MissionListener>();
		this.selfReportPerPlayer = new HashMap<String, MissionSelfReport>();
		this.canShowSelfReport = false;
		this.entitiesToRemove = new HashMap<Entity, Long>();
		MinecraftForge.EVENT_BUS.register(this);
	}
	
	@SubscribeEvent
	public void PlayerDeath(LivingDeathEvent event) {
		if (!event.getEntity().world.isRemote && event.getEntity() instanceof EntityPlayer) {		
			event.setCanceled(true);
			this.onPlayerDeath();
		}				
	}
	
	@SubscribeEvent(priority = EventPriority.HIGHEST)
	public void CommandEvents(CommandEvent evt) {
		System.out.println("================>COMMAND");		
		System.out.println(evt.getSender().getName());
		System.out.println(evt.getCommand().getName());
		if(evt.getSender() instanceof EntityPlayer){
			System.out.println("============>Player sent event");
		}		
	}
	
	/**
	 * Method called after if the player dies
	 */
	protected abstract void onPlayerDeath();

	/**
	 * Gets the ID that identifies the type of mission
	 * @return
	 */
	protected abstract int getID();

	/**
	 * Adds listener to be notified upon relevant mission events
	 * @param listener - Mission listener object
	 */
	public void addListener(MissionListener listener) {
		this.listeners.add(listener);
	}

	/**
	 * Create phases of the mission. Implemented in the subclasses.
	 */
	protected abstract void createPhases();

	/**
	 * Initialize data for the mission.
	 * @param world - Mission world
	 */
	public void init(World world) {
		this.numberOfPhasesCompleted = 0;
		this.phases = new ArrayList<MissionPhase>();
		this.createPhases();
		if (!this.phases.isEmpty()) {
			this.currentPhase = this.phases.get(0);
		}	
		this.initMalmoModClientAndServerMission();
	};

	/**
	 * Initialize client and server mission objects in the active MalmoMod instance
	 */
	public void initMalmoModClientAndServerMission() {
		MalmoMod.instance.getClient().setTomcatClientMission(this.getClientMissionInstance());
		MalmoMod.instance.getServer().setTomcatServerMission(this);	
		QuitProducer quitProducerHandler = new QuitProducer();
		this.addListener(quitProducerHandler);
		MalmoMod.instance.getServer().addQuitProducer(quitProducerHandler);
	}

	/**
	 * Retrieves a new instance of the mission that should run in the client side
	 * @return
	 */
	protected abstract ClientMission getClientMissionInstance();

	/**
	 * Updates the mission from time to time. This method is called at every tick.
	 * @param world - Mission world
	 */
	public void update(World world) {
		this.initSelfReports(world);
		if (this.timeLimitInSeconds > 0) {
			int remainingSeconds = this.getRemainingSeconds(world);

			if(remainingSeconds >= 0 || this.timeLimitInSeconds == -1) {
				this.drawingHandler.drawCountdown(remainingSeconds, REMAINING_SECONDS_ALERT);
				this.updateOnRunningState(world);
			} else {
				this.onTimeOut();
			}
		} else {
			this.updateOnRunningState(world);
		}		
	}
	
	/**
	 * Updates the mission if it's not timed out
	 */
	private void updateOnRunningState(World world) {
		this.removeEntities(world);
		this.updateCurrentPhase(world);
		this.updateScene(world);
		this.showSelfReportScreen(world);
	}
	
	private void removeEntities(World world) {
		Set<Entity> entities = this.entitiesToRemove.keySet();
		for (Entity entity : entities) {
			int remainingTime = Converter.getRemainingTimeInSeconds(world, this.entitiesToRemove.get(entity), 
					ENTITY_DELETION_DELAY);
			if (remainingTime <= 0) {
				world.removeEntity(entity);
				this.entitiesToRemove.remove(entity);
			}			
		}
	}

	/**
	 * Create a self-report for each player in the game
	 * @param world - Minecraft world
	 */
	private void initSelfReports(World world) {
		if(this.hasSelfReport()) {
			if(this.selfReportPerPlayer.isEmpty()) {
				Date missionStartTime = new Date();
				for(EntityPlayer player : world.playerEntities) {
					MissionSelfReport selfReport = new MissionSelfReport(this.getID(), missionStartTime, player.getName());
					this.selfReportPerPlayer.put(player.getName(), selfReport);
				}
			}
		}
	}

	/**
	 * Retrieves the remaining seconds until the end of the mission
	 * @return
	 */
	protected int getRemainingSeconds(World world) {
		if (this.initialWorldTime == 0) {
			this.initialWorldTime = world.getTotalWorldTime();
		}	

		return Converter.getRemainingTimeInSeconds(world, this.initialWorldTime, this.timeLimitInSeconds);		    
	}

	/**
	 * Update the current phase of the mission if there's any 
	 * @param world
	 */
	protected void updateCurrentPhase(World world) {
		if (this.currentPhase != null) {
			this.currentPhase.update(world);
		}
	}

	/**
	 * Updates the mission scene programmatically. Must be implemented at the subclass level. 
	 * @param world
	 */
	protected abstract void updateScene(World world);

	private void showSelfReportScreen(World world) {
		if(this.hasSelfReport()) {
			long elapsedTime = Converter.getElapsedTimeInSeconds(world, this.initialWorldTime);
			if(elapsedTime%this.selfReportPromptTimeInSeconds == this.selfReportPromptTimeInSeconds/2){
				// It's been passed enough time since last self-report screen was prompted.
				// Allow the game to prompt another self-report screen when it comes the time to do so 
				this.canShowSelfReport = true;
			}

			if(elapsedTime%this.selfReportPromptTimeInSeconds == 0 && this.canShowSelfReport) {
				SelfReportContent content = this.getSelfReportContent(world);
				MalmoMod.network.sendToAll(new TomcatMessaging.TomcatMessage(TomcatMessageType.SHOW_SELF_REPORT, new TomcatMessageData(content)));
				this.canShowSelfReport = false;
			}
		}
	}

	/**
	 * Checks whether the mission has self-report or not
	 * @return
	 */
	protected abstract boolean hasSelfReport();

	/**
	 * Gets the self-report content (questions and choices) for the mission
	 * @return
	 */
	protected abstract SelfReportContent getSelfReportContent(World world);
	
	@Override
	public void emotionProvided(EmotionHandler.Emotion emotion) {
		this.currentEmotion = emotion;		
	}	

	/**
	 * Adds a phase to the mission
	 * @param phase - Mission phase
	 */
	public void addPhase(MissionPhase phase) {
		phase.addListener(this);
		this.phases.add(phase);		
	}

	@Override
	public void phaseCompleted() {
		this.beforePhaseTrasition();
		this.numberOfPhasesCompleted++;
		this.currentPhase = null;
		if (this.numberOfPhasesCompleted < this.phases.size()) {
			this.currentPhase = this.phases.get(this.numberOfPhasesCompleted);
		} else {
			this.afterLastPhaseCompletion();
		}	
	}

	/**
	 * Method called after the current phase is completed and before the next phase is selected. It can be implemented in
	 * a concrete mission class to provide specific logic.
	 */
	protected abstract void beforePhaseTrasition();

	/**
	 * Method called after the last phase of the mission is completed.
	 */
	protected abstract void afterLastPhaseCompletion();

	/**
	 * Method called after the the total time for the mission has passed.
	 */
	protected abstract void onTimeOut();

	/**
	 * Handle message from the client side
	 */
	public void handleMessageFromClient(TomcatMessage message) {		
		switch (message.getMessageType()) {
		case OPEN_SCREEN_DISMISSED:
			this.currentPhase.openScreenDismissed();
			break;

		case SELF_REPORT_ANSWERED:
			String playerName = message.getMessageData().getPlayerName();
			SelfReportContent content = message.getMessageData().getSelfReport();
			this.selfReportPerPlayer.get(playerName).addContent(content);
			break;
		
		case DISPLAY_INSTRUCTIONS:
			this.currentPhase.showInstructions();
			
		case CONNECTION_ERROR:
			this.onTimeOut();
			
		default:
			break;
		}				

	}

	public abstract PosAndDirection getPlayersInitialPositionAndDirection(EntityPlayerMP player);
	
	/**
	 * Notifies listeners about the mission ending
	 */
	protected void notifyAllAboutMissionEnding(String exitCode) {		
		for (MissionListener listener : this.listeners) {
			listener.missionEnded(exitCode);			
		}
	}
	
	/** 
	 * Save files and other pending stuff that should be flushed when the mission ends.
	 * This method must be called by the subclasses upon the end of the mission.
	 */
	protected void cleanup() {
		this.saveSelfReports();
	}

	/**
	 * Save self-report content to a file
	 * @param selfReportContent - Content of the self-report
	 */
	private void saveSelfReports() {
		if(this.hasSelfReport()) {
			this.createSelfReportsFolder();
			for(MissionSelfReport selfReport : this.selfReportPerPlayer.values()) {
				if(selfReport.hasContent()) {
					String path = this.getSelfReportPath(selfReport);
					this.writeSelfReportToFile(path, selfReport);
				}
			}	
		}
	}

	/**
	 * Creates self-reports folder if it doesn't exist already
	 */
	private void createSelfReportsFolder() {
		File folder = new File(SELF_REPORT_FOLDER);
		if(!folder.exists()) {
			folder.mkdir();
		}
	}

	/**
	 * Get the filename for a given self-report based on some of its info
	 */
	private String getSelfReportPath(MissionSelfReport selfReport) {
		DateFormat dateFormat = new SimpleDateFormat("yyyy_mm_dd_hh_mm_ss");
		String path = String.format("%s/%d_%s_%s.json", SELF_REPORT_FOLDER, selfReport.getMissionID(), selfReport.getPlayerID(), 
				dateFormat.format(selfReport.getMissionStartTime()));
		return path;
	}
	
	/**
	 * Write all self-reports from a player in a mission to a file
	 * @param path - Filename of the self-report file
	 * @param selfReport - Self-report object
	 */
	private void writeSelfReportToFile(String path, MissionSelfReport selfReport) {
		try {       	
			FileWriter fileWriter = new FileWriter(path);
			Gson gson = new GsonBuilder().create();
			String json = gson.toJson(selfReport);
			fileWriter.write(json);
			fileWriter.close();
		} catch (IOException e) {
			e.printStackTrace();
		}  
	}

	/**
	 * Defines the duration of the mission in seconds 
	 * @param timeLimitInSeconds - Time in seconds until the end of the mission
	 */
	public void setTimeLimitInSeconds(long timeLimitInSeconds) {
		this.timeLimitInSeconds = timeLimitInSeconds;
	}

	/**
	 * Defines the time interval to wait until prompting a self-report screen to the player
	 * @param selfReportPromptTimeInSeconds - Time to wait until show a self-report screen
	 */
	public void setSelfReportPromptTimeInSeconds(long selfReportPromptTimeInSeconds) {
		this.selfReportPromptTimeInSeconds = selfReportPromptTimeInSeconds;
	}
	
	/**
	 * Adds an entity to be deleted after certain amount of time
	 * @param entity - Entity to be removed
	 * @param delayInSeconds - Time (in seconds) to wait until removing the entity
	 */
	public void addToDeletion(Entity entity, long worldTime) {
		this.entitiesToRemove.put(entity, worldTime);
	}
	
}
