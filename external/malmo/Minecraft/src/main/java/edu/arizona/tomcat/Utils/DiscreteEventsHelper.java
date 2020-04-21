package edu.arizona.tomcat.Utils;

import net.minecraft.entity.monster.EntityMob;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.math.BlockPos;
import edu.arizona.tomcat.events.AttackDiscreteEvent;

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
