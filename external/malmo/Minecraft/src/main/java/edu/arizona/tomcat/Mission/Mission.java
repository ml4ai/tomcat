package edu.arizona.tomcat.Mission;

import java.util.ArrayList;

import edu.arizona.tomcat.Emotion.EmotionHandler;
import edu.arizona.tomcat.Mission.gui.FeedbackListener;
import edu.arizona.tomcat.Utils.Converter;
import edu.arizona.tomcat.World.DrawingHandler;
import net.minecraft.client.Minecraft;
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
		this.numberOfPhasesCompleted = 0;
		this.phases = new ArrayList<MissionPhase>();
		this.createPhases();
		if (!this.phases.isEmpty()) {
			this.currentPhase = this.phases.get(0);
		}
	}
	
	/**
	 * Create phases of the mission. Implemented in the subclasses.
	 */
	protected abstract void createPhases();
	
	/**
	 * Draws the initial objects and entities of the mission.
	 * @param world - Mission world
	 */
	public abstract void init(World world);
	
	/**
	 * Updates the mission from time to time. This method is called at every tick.
	 * @param world - Mission world
	 */
	public void update(World world) {
	//	this.drawingHandler.drawCountdown(this.getRemainingSeconds(), REMAINING_SECONDS_ALERT);
		this.updateCurrentPhase(world);
		this.updateScene(world);
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
		this.numberOfPhasesCompleted++;
		this.currentPhase = null;
		if (this.numberOfPhasesCompleted < this.phases.size()) {
			this.currentPhase = this.phases.get(this.numberOfPhasesCompleted);
		}		
	}
	
}
