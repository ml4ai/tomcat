package edu.arizona.tomcat.Utils;

import com.google.gson.internal.LinkedTreeMap;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.math.BlockPos;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Map;

/**
 * This class holds static methods to print the discrete events. It currently
 * prints the output to terminal.
 */
public class DiscreteEventsHelper {

  /**
   * When called, this method will print the occurence of the event to the
   * terminal. The Blockpos passed is the coordinate at which the event
   * was triggered, and the playerIn is the player who triggered the event.
   *
   * @param pos      - Position of event
   * @param playerIn -  The player who triggered the event
   */
  public static void
  printEventOccurence(BlockPos pos, EntityPlayer playerIn, String event) {
    String coordinates = createCoordinateString(pos);

    DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
    Date date = new Date();
    String dateTime = dateFormat.format(date); // Date and Time

    Map<String, String> output = new LinkedTreeMap<String, String>();

    output.put("Event Type", event);
    output.put("Event caused by", playerIn.getDisplayNameString());
    output.put("Event Coordinates", coordinates);
    output.put("Occurence Time", dateTime);

    // The output is placed in a map above. The code below is only for temporary
    // printing to terminal.

    System.out.println("+---EVENT REPORT---+");
    for (String key : output.keySet()) {
      System.out.println(key + ": " + output.get(key));
    }
    System.out.println();
    System.out.println();
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
  printAttackEventOccurence(BlockPos pos, Entity enemy, EntityPlayer playerIn) {

    String playerName = playerIn.getDisplayNameString();
    String enemyName = enemy.getName();

    String event = playerName + " killed " + enemyName;
    if (enemy.isEntityAlive()) {
      event = playerName + " attacked " + enemyName;
    }

    String coordinates = createCoordinateString(pos);

    DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
    Date date = new Date();
    String dateTime = dateFormat.format(date); // Date and Time

    Map<String, String> output = new LinkedTreeMap<String, String>();

    output.put("Event Type", event);
    output.put("Event caused by", playerName);
    output.put("Event Coordinates", coordinates);
    output.put("Occurence Time", dateTime);

    // The output is placed in a map above. The code below is only for temporary
    // printing to terminal.

    System.out.println("+---EVENT REPORT---+");
    for (String key : output.keySet()) {
      System.out.println(key + ": " + output.get(key));
    }
    System.out.println();
    System.out.println();
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
