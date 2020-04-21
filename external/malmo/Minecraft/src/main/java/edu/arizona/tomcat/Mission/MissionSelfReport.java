package edu.arizona.tomcat.Mission;

import edu.arizona.tomcat.Mission.gui.SelfReportResponses;

public class MissionSelfReport {

  private String version;
  private int missionId;
  private String player;
  private String timestamp;
  private SelfReportResponses responses;

  /**
   * Constructor
   * @param missionID - Code that uniquely identifies the type of mission
   * running
   * @param player - Name of the player who answered the self-report
   * @param timestamp - When the self-report was presented
   */
  public MissionSelfReport(int missionId, String player, String timestamp) {
    this.missionId = missionId;
    this.player = player;
    this.timestamp = timestamp;
  }

  /**
   * Checks if self-report has responses
   * @return
   */
  public boolean hasResponses() { return this.responses != null; }

  /**
   * Sets a responses to the self-report
   * @param responses
   */
  public void setResponses(SelfReportResponses responses) {
    this.responses = responses;
  }

  /**
   * Gets the mission Id
   * @return
   */
  public int getMissionId() { return missionId; }

  /**
   * Gets the timestamp when the self-report was presented to the player
   * @return
   */
  public String getTimestamp() { return timestamp; }

  /**
   * Gets the name of the player who answered the self-reports
   * @return
   */
  public String getPlayer() { return player; }

  /**
   * Retrieves the version of the self-report document
   * @return
   */
  public String getVersion() { return version; }

  /**
   * Sets the version if the self-report document
   * @param version
   */
  public void setVersion(String version) { this.version = version; }
}
