package edu.arizona.tomcat.Utils;

import com.microsoft.Malmo.Utils.TimeHelper;

import net.minecraft.client.Minecraft;

public class Converter {
	
	/**
	 * Calculates the remaining time in seconds given the initial and current time in ticks
	 * @param initialTime - Initial time in ticks
	 * @param timeLimitInSeconds - Time limit in seconds
	 * @return
	 */
	public static final int getRemainingTimeInSeconds(long initialTime, long timeLimitInSeconds) {
		long currentWorldTime = Minecraft.getMinecraft().world.getTotalWorldTime();
		long timeElapsedInWorldTicks = currentWorldTime - initialTime;
		float timeRemainingInMs = timeLimitInSeconds * TimeHelper.MillisecondsPerSecond - (timeElapsedInWorldTicks * TimeHelper.MillisecondsPerWorldTick);

		return (int) Math.ceil(timeRemainingInMs / TimeHelper.MillisecondsPerSecond);
	}
	
	
}
