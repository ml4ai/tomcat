package edu.arizona.tomcat.Mission;

import edu.arizona.tomcat.Mission.gui.SelfReportContent;
import java.util.ArrayList;
import java.util.Date;

public class MissionSelfReport {

  private int missionID;
  private Date missionStartTime;
  private String playerID;
  private ArrayList<SelfReportContent> contents;

  /**
   * Constructor
   * @param missionID - Code that uniquely identifies the type of mission
   * running
   * @param missionStartTime - Date and time when the mission started
   */
  public MissionSelfReport(int missionID,
                           Date missionStartTime,
                           String playerID) {
    this.missionID = missionID;
    this.missionStartTime = missionStartTime;
    this.playerID = playerID;
    this.contents = new ArrayList<SelfReportContent>();
  }

  /**
   * Checks if self-report has content
   * @return
   */
  public boolean hasContent() { return !this.contents.isEmpty(); }

  /**
   * Adds a self-report content to the set of self-reports contents of a mission
   * @param selfReport
   */
  public void addContent(SelfReportContent content) {
    this.contents.add(content);
  }

  /**
   * Gets the mission ID
   * @return
   */
  public int getMissionID() { return missionID; }

  /**
   * Gets the mission start datetime
   * @return
   */
  public Date getMissionStartTime() { return missionStartTime; }

  /**
   * Gets the ID of the player who answered the self-reports
   * @return
   */
  public String getPlayerID() { return playerID; }
}
