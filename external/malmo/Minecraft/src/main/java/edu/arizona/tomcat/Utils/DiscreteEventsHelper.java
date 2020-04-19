package edu.arizona.tomcat.Utils;

import net.minecraft.entity.monster.EntityMob;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.math.BlockPos;

import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * This class holds static methods to print the discrete events. It writes the
 * output to saves/discrete_events/Discrete_Events.json
 */
public class DiscreteEventsHelper {

  private static final String DISCRETE_EVENT_REPORTS_FOLDER =
      "saves/discrete_events";
  private static int counter = 0;

  public static void createDiscreteEventsOutputFolder() {
    OutputDataHandler.createFolder(DISCRETE_EVENT_REPORTS_FOLDER);
  }

  private static void writeJSONOutput(String filename, Object discreteEvent) {
    if (counter % 2 == 0) {
      createDiscreteEventsOutputFolder();
      try {
        OutputDataHandler.appendToJsonFile(
            DISCRETE_EVENT_REPORTS_FOLDER, filename, discreteEvent);
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }
    counter++;
  }

  /**
   * When called, this method will print the occurrence of the event to the
   * terminal. The BlockPos passed is the coordinate at which the event
   * was triggered, and the playerIn is the player who triggered the event.
   *
   * @param pos      - Position of event
   * @param playerIn -  The player who triggered the event
   */
  public static void
  writeBlockEvent(BlockPos pos, EntityPlayer playerIn, String eventName) {

    DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
    Date date = new Date();

    String timestamp = dateFormat.format(date); // Date and Time
    String coordinates = createCoordinateString(pos);
    String playerName = "";
    if (playerIn != null) {
      playerName = playerIn.getDisplayNameString();
    }

    BlockDiscreteEvent event =
        new BlockDiscreteEvent(eventName, timestamp, coordinates);
    writeJSONOutput(getFilename(playerName), event);
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
  public static void
  writeAttackEvent(BlockPos pos, EntityMob enemy, EntityPlayer playerIn) {

    // Player and Enemy Info
    String playerName = playerIn.getDisplayNameString();
    String playerHealth = playerIn.getHealth() + "/" + playerIn.getMaxHealth();
    String enemyName = enemy.getName();
    String enemyHealth = "0.0"
                         + "/" + enemy.getMaxHealth();

    String eventName = "enemy_killed";

    if (enemy.isEntityAlive()) {
      eventName = "enemy_attacked";
      enemyHealth = enemy.getHealth() + "/" + enemy.getMaxHealth();
    }

    // Logistics Info
    DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
    Date date = new Date();

    String timestamp = dateFormat.format(date); // Date and Time
    String coordinates = createCoordinateString(pos);

    AttackDiscreteEvent event = new AttackDiscreteEvent(eventName,
                                                        timestamp,
                                                        coordinates,
                                                        playerHealth,
                                                        enemyName,
                                                        enemyHealth);

    writeJSONOutput(getFilename(playerName), event);
  }

  private static String getFilename(String playerName) {
    String filename =
        String.format("discrete_events_player_%s.json", playerName);
    return filename;
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
}
