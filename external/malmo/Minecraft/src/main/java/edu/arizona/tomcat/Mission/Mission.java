package edu.arizona.tomcat.Mission;

import java.util.ArrayList;

import com.microsoft.Malmo.MalmoMod;
import com.microsoft.Malmo.Schemas.PosAndDirection;

import edu.arizona.tomcat.Emotion.EmotionHandler;
import edu.arizona.tomcat.Messaging.TomcatMessaging.TomcatMessage;
import edu.arizona.tomcat.Mission.Client.ClientMission;
import edu.arizona.tomcat.Mission.gui.FeedbackListener;
import edu.arizona.tomcat.Utils.Converter;
import edu.arizona.tomcat.World.DrawingHandler;
import net.minecraft.client.Minecraft;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.world.World;

public abstract class Mission implements FeedbackListener, PhaseListener {

	protected static final int REMAINING_SECONDS_ALERT = 30;

	protected long timeLimitInSeconds;
	protected long initialWorldTime;
	protected DrawingHandler drawingHandler;
	protected EmotionHandler.Emotion currentEmotion;
	protected ArrayList<MissionPhase> phases;
	protected MissionPhase currentPhase;
	protected int numberOfPhasesCompleted; 

	/**
	 * Abstract constructor for initialization of the drawing handler
	 */
	protected Mission() {
		this.drawingHandler = DrawingHandler.getInstance();
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
		if (this.timeLimitInSeconds > 0) {
			int remainingSeconds = this.getRemainingSeconds();

			if(remainingSeconds >= 0 || this.timeLimitInSeconds == -1) {
				this.drawingHandler.drawCountdown(remainingSeconds, REMAINING_SECONDS_ALERT);
				this.updateCurrentPhase(world);
				this.updateScene(world);
			} else {
				this.onTimeOut();
			}
		} else {
			this.updateCurrentPhase(world);
			this.updateScene(world);
		}
	}

	/**
	 * Retrieves the remaining seconds until the end of the mission
	 * @return
	 */
	protected int getRemainingSeconds() {
		long currentWorldTime = Minecraft.getMinecraft().world.getTotalWorldTime();

		if (this.initialWorldTime == 0) {
			this.initialWorldTime = currentWorldTime;
		}	

		return Converter.getRemainingTimeInSeconds(this.initialWorldTime, this.timeLimitInSeconds);		    
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


	/**
	 * Defines the duration of the mission in seconds 
	 * @param timeLimitInSeconds - Time in seconds until the end of the mission
	 */
	public void setTimeLimitInSeconds(long timeLimitInSeconds) {
		this.timeLimitInSeconds = timeLimitInSeconds;
	}

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
	 * Method called after the the total time for the mission has passes.
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

		default:
			break;
		}				

	}
	
	public abstract PosAndDirection getPlayersInitialPositionAndDirection(EntityPlayerMP player);

}
