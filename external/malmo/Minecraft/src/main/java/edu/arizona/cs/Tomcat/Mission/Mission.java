package edu.arizona.cs.Tomcat.Mission;

import com.microsoft.Malmo.Utils.TimeHelper;

import edu.arizona.cs.Tomcat.Emotion.EmotionHandler;
import edu.arizona.cs.Tomcat.Emotion.EmotionHandler.Emotion;
import edu.arizona.cs.Tomcat.Mission.gui.FeedbackListener;
import edu.arizona.cs.Tomcat.World.DrawingHandler;
import net.minecraft.client.Minecraft;
import net.minecraft.world.World;

public abstract class Mission implements FeedbackListener {
	
	protected static final int REMAINING_SECONDS_ALERT = 30;
	
	protected long timeLimitInSeconds;
	protected long initialWorldTime;
	protected DrawingHandler drawingHandler;
	protected EmotionHandler.Emotion currentEmotion;
	
	/**
	 * Abstract constructor for initialization of the drawing handler
	 */
	protected Mission() {
		this.drawingHandler = DrawingHandler.getInstance();
	}
	
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
		this.drawingHandler.drawCountdown(this.getRemainingSeconds(), REMAINING_SECONDS_ALERT);
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
		
		long timeElapsedInWorldTicks = currentWorldTime - this.initialWorldTime;
		float timeRemainingInMs = this.timeLimitInSeconds * TimeHelper.MillisecondsPerSecond - (timeElapsedInWorldTicks * TimeHelper.MillisecondsPerWorldTick);
				
		return (int) Math.ceil(timeRemainingInMs / TimeHelper.MillisecondsPerSecond);	    
	}
	
	/**
	 * Defines the duration of the mission in seconds 
	 * @param timeLimitInSeconds - Time in seconds until the end of the mission
	 */
	public void setTimeLimitInSeconds(long timeLimitInSeconds) {
		this.timeLimitInSeconds = timeLimitInSeconds;
	}
	
	@Override
	public void emotionProvided(Emotion emotion) {
		this.currentEmotion = emotion;		
	}		
	
}
