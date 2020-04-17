package edu.arizona.tomcat.Mission.gui;

import edu.arizona.tomcat.Mission.MissionSelfReport;
import edu.arizona.tomcat.Utils.OutputDataHandler;
import java.io.IOException;

public class SelfReportFileHandler {

  private static final String SELF_REPORT_OUTPUT_FOLDER = "saves/self_reports";

  /**
   * Appends the self-report responses to a .json file with the player's
   * self-report responses in the mission
   * @param missionSelfReport
   */
  public static void writeSelfReport(MissionSelfReport missionSelfReport) {
    try {
      createSelfReportOutputFolder();
      String filename = getSelfReportFilename(missionSelfReport);
      OutputDataHandler.appendToJsonFile(
          SELF_REPORT_OUTPUT_FOLDER, filename, missionSelfReport);
    }
    catch (IOException e) {
      e.printStackTrace();
    }
  }

  /**
   * Creates an output folder to save the self-reports if it does not exist yet
   */
  public static void createSelfReportOutputFolder() {
    OutputDataHandler.createFolder(SELF_REPORT_OUTPUT_FOLDER);
  }

  /**
   * Retrieves the self-report output file for a given player
   * @param missionSelfReport
   * @return
   */
  private static String
  getSelfReportFilename(MissionSelfReport missionSelfReport) {
    String path = String.format("self_report_player_%s.json",
                                missionSelfReport.getPlayer());
    return path;
  }
}
