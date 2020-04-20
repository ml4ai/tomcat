package edu.arizona.tomcat.Utils;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;
import com.microsoft.Malmo.Utils.TimeHelper;
import java.lang.reflect.Type;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Map;
import java.util.TimeZone;
import net.minecraft.world.World;

public class Converter {

  /**
   * Calculates the number of world ticks passed since the beginning of the
   * mission
   * @param world - Minecraft world
   * @param initialTime - Initial time in ticks
   * @return
   */
  private static final long getElapsedTime(World world, long initialTime) {
    long currentWorldTime = world.getTotalWorldTime();
    return (currentWorldTime - initialTime);
  }

  /**
   * Calculates the number of seconds passed since the beginning of the mission
   * @param world - Minecraft world
   * @param initialTime - Initial time in ticks
   * @return
   */
  public static final int getElapsedTimeInSeconds(World world,
                                                  long initialTime) {
    long elapsedTime = getElapsedTime(world, initialTime);
    return (int)Math.ceil((elapsedTime * TimeHelper.MillisecondsPerWorldTick) /
                          TimeHelper.MillisecondsPerSecond);
  }

  /**
   * Calculates the remaining time in seconds given the initial and current time
   * in ticks
   * @param initialTime - Initial time in ticks
   * @param timeLimitInSeconds - Time limit in seconds
   * @return
   */
  public static final int getRemainingTimeInSeconds(World world,
                                                    long initialTime,
                                                    long timeLimitInSeconds) {
    long elapsedTime = getElapsedTime(world, initialTime);
    float timeRemainingInMs =
        timeLimitInSeconds * TimeHelper.MillisecondsPerSecond -
        (elapsedTime * TimeHelper.MillisecondsPerWorldTick);
    return (int)Math.ceil(timeRemainingInMs / TimeHelper.MillisecondsPerSecond);
  }

  /**
   * Converts seconds to MM:SS
   * @param seconds - Number of seconds
   * @return
   */
  public static final String secondsToString(long seconds,
                                             boolean hideSecondsIfZero) {
    String time = "";

    if (seconds <= 60) {
      time = Long.toString(seconds) + " seconds";
    }
    else {
      long minutes = seconds / 60;
      seconds = seconds % 60;

      if (hideSecondsIfZero && seconds == 0) {
        time = minutes + " minutes";
      }
      else {
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
      Type type = new TypeToken<Map<String, String>>() {}.getType();
      map = jsonObject.fromJson(json, type);
    }
    return map;
  }

  /**
   * Retrieves the current time in ISO-8601 extended format
   * @return
   */
  public static String getCurrentTimestamp() {
    TimeZone timeZone = TimeZone.getTimeZone("UTC");
    DateFormat dateFormat = new SimpleDateFormat(
        "yyyy-MM-dd'T'HH:mm'Z'"); // Quoted "Z" to indicate UTC, no timezone
                                  // offset
    dateFormat.setTimeZone(timeZone);
    return dateFormat.format(new Date());
  }
}
