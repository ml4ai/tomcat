package edu.arizona.cs.Tomcat.Utils;

import com.microsoft.Malmo.Utils.TimeHelper;

import net.minecraft.client.Minecraft;

public class Converter {

	public static final int getRemainingTimeInSeconds(long initialTime, long timeLimitInSeconds) {
		long currentWorldTime = Minecraft.getMinecraft().world.getTotalWorldTime();
		long timeElapsedInWorldTicks = currentWorldTime - initialTime;
		float timeRemainingInMs = timeLimitInSeconds * TimeHelper.MillisecondsPerSecond - (timeElapsedInWorldTicks * TimeHelper.MillisecondsPerWorldTick);

		return (int) Math.ceil(timeRemainingInMs / TimeHelper.MillisecondsPerSecond);
	}
	
	
}
