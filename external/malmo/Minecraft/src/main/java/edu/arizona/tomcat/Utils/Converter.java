package edu.arizona.tomcat.Utils;

import java.lang.reflect.Type;
import java.util.Map;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;
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

	/**
	 * Converts seconds to MM:SS
	 * @param seconds - Number of seconds
	 * @return
	 */
	public static final String secondsToString(long seconds, boolean hideSecondsIfZero) {
		String time = "";

		if(seconds <= 60) {
			time = Long.toString(seconds) + " seconds";
		} else {
			long minutes = seconds / 60;
			seconds = seconds % 60;

			if (hideSecondsIfZero && seconds == 0) {
				time = minutes + " minutes";
			} else {
				time = minutes + ":" + String.format("%02d", seconds) + " minutes";
			}
		}

		return time;
	}

	/**
	 * Converts a map object to a Json string
	 * @param map - Dictionary (key, value pairs)
	 * @return
	 */
	public static final String mapToJson(Map<String, String> map) {
		String string = null;
		if (map != null) {
			GsonBuilder jsonMapBuilder = new GsonBuilder();
			Gson jsonObject = jsonMapBuilder.create();		
			string = jsonObject.toJson(map);
		}
		return string;
	}

	/**
	 * Converts a Json string to a map object
	 * @param json - Json string
	 * @return
	 */
	public static final Map<String, String> jsonToMap(String json) {
		Map<String, String> map = null;
		if (json != null) {			
			Gson jsonObject = new Gson();	
			Type type = new TypeToken<Map<String, String>>(){}.getType();
			map = jsonObject.fromJson(json, type);			
		}
		return map;
	}

}
