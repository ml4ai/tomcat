package edu.arizona.tomcat.Utils;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.internal.LinkedTreeMap;
import net.minecraft.entity.monster.EntityMob;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.math.BlockPos;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Map;

/**
 * This class holds static methods to print the discrete events. It writes the
 * output to saves/discrete_events/Discrete_Events.json
 */
public class DiscreteEventsHelper {

  private static final String DISCRETE_EVENT_REPORTS_FOLDER =
      "saves/discrete_events";
  private static int counter = 0;

  /**
   * When called, this method will print the occurrence of the event to the
   * terminal. The BlockPos passed is the coordinate at which the event
   * was triggered, and the playerIn is the player who triggered the event.
   *
   * @param pos      - Position of event
   * @param playerIn -  The player who triggered the event
   */
  public static void
  printEventOccurrence(BlockPos pos, EntityPlayer playerIn, String event) {
    String coordinates = createCoordinateString(pos);

    DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
    Date date = new Date();
    String dateTime = dateFormat.format(date); // Date and Time

    Map<String, String> output = new LinkedTreeMap<String, String>();

    output.put("Event Type", event);
    if(playerIn != null) {
      output.put("Event caused by", playerIn.getDisplayNameString());
    }
    output.put("Event Coordinates", coordinates);
    output.put("Occurrence Time", dateTime);

    saveDiscreteEventReport(output);
  }

  /**
   * When called, this method will print the occurrence of an attack event to
   * the terminal. The details of the attack event consist of who the player was
   * and who they attacked. The method can figure out whether an attack killed
   * the enemy.
   *
   * @param pos      - Position of event
   * @param playerIn -  The player who triggered the event
   */
  public static void printAttackEventOccurrence(BlockPos pos,
                                                EntityMob enemy,
                                                EntityPlayer playerIn) {

    String playerName = playerIn.getDisplayNameString();
    String playerHealth = playerIn.getHealth() + "/" + playerIn.getMaxHealth();
    String enemyName = enemy.getName();

    String event = playerName + " killed " + enemyName;
    String enemyHealth = "0.0" + "/" + enemy.getMaxHealth();
    if (enemy.isEntityAlive()) {
      event = playerName + " attacked " + enemyName;
      enemyHealth = enemy.getHealth() + "/" + enemy.getMaxHealth();
    }

    String coordinates = createCoordinateString(pos);

    DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
    Date date = new Date();
    String dateTime = dateFormat.format(date); // Date and Time

    Map<String, String> output = new LinkedTreeMap<String, String>();

    output.put("Event Type", event);
    output.put("Current Enemy Health", enemyHealth);
    output.put("Current Player Health", playerHealth);
    output.put("Event caused by", playerName);
    output.put("Event Coordinates", coordinates);
    output.put("Occurrence Time", dateTime);

    saveDiscreteEventReport(output);
  }

  /**
   * Returns the position information as a String.
   *
   * @param pos - The coordinates as a BlockPos
   * @return A String representation
   */
  private static String createCoordinateString(BlockPos pos) {
    int x = pos.getX(), y = pos.getY(), z = pos.getZ(); // event coordinates
    String coordinates = "X: " + x + " "
                         + "Y: " + y + " "
                         + "Z: " + z;
    return coordinates;
  }

  /**
   * Calls the necessary methods to save the discrete event report
   * passed as a Java Map to the Discrete_Events.json file.
   *
   * @param mapReport - The event report
   */
  private static void saveDiscreteEventReport(Map<String, String> mapReport) {
    createDiscreteEventsFolder();
    String path = getDiscreteEventstPath();
    writeDiscreteEventsToFile(path, mapReport);
    counter++;
  }

  /**
   * Creates discrete_events folder if it doesn't exist already
   */
  private static void createDiscreteEventsFolder() {
    File folder = new File(DISCRETE_EVENT_REPORTS_FOLDER);
    if (!folder.exists()) {
      folder.mkdir();
    }
  }

  /**
   * All discrete events are written to the path returned by this method.
   * Currently, discrete event occurrences are appended onto the same file.
   */
  private static String getDiscreteEventstPath() {
    String path = DISCRETE_EVENT_REPORTS_FOLDER + "/discrete_events.json";
    return path;
  }

  /**
   * Writes output to .json file.
   *
   * @param path      - The filepath where the event is to be appended.
   * @param mapReport - The event report.
   */
  private static void writeDiscreteEventsToFile(String path,
                                                Map<String, String> mapReport) {
    try {
      if (counter % 2 == 0) {
        FileWriter fileWriter = new FileWriter(path, true);
        Gson gson = new GsonBuilder().create();
        String json = gson.toJson(mapReport) + "\n\n";
        fileWriter.write(json);
        fileWriter.close();
      }
      else {
        ;
      }
    }
    catch (IOException e) {
      e.printStackTrace();
    }
  }
}
